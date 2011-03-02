trait GlobalInteraction
trait LocalInteraction

type Global = List[GlobalInteraction]
type Local = List[LocalInteraction]

case class Branch(label: String, body: Global)

case class Interaction(from: String, to: String, msgSig: String) extends GlobalInteraction
case class Choice(from: String, to: String, branches: List[Branch]) extends GlobalInteraction {
  def concat(global: Global): Choice =
    copy(branches = branches map (b => b.copy(body = b.body:::global)))
}
case class Recursion(label: String, body: Global) extends GlobalInteraction 
case class RecursionLabel(label: String) extends GlobalInteraction with LocalInteraction
case class Parallel(p1: Global, p2: Global) extends GlobalInteraction

case class LocalBranch(label: String, body: Local)

case class SendChoice(to: String, branches: List[LocalBranch]) extends LocalInteraction
case class ReceiveChoice(from: String, branches: List[LocalBranch]) extends LocalInteraction {
  def concat(local: Local): ReceiveChoice =
    copy(branches = branches map (b => b.copy(body = b.body:::local)))
}
case class LocalRecursion(label: String, body: Local) extends LocalInteraction
case class Send(to: String, msgSig: String) extends LocalInteraction
case class Receive(from: String, msgSig: String) extends LocalInteraction

def pretty(local: Local) {
  if (!local.isEmpty) {
    println("{")
    local foreach { i =>
      i match {
        case ReceiveChoice(from, branches) => 
          println("ReceiveChoice from "+from+" {")
          branches foreach { b =>
            print(b.label+":"); pretty(b.body); println()
          }
          print("}")
        case _ => print(i)
      }
      println(";")
    }
    println("}")
  }
}

val testPrefix = List(
  Choice("A", "B", List(
    Branch("M1", List(
      Interaction("C", "D", "M0"),
      Interaction("B", "C", "M3")
    )),
    Branch("M2", List(
      Interaction("C", "D", "M0"),
      Interaction("B", "C", "M4")
    ))
  ))
)

val testNested = List(
  Choice("A", "B", List(
    Branch("M1", List(
      Choice("B", "C", List(
        Branch("M1", List(

        )),
        Branch("M2", List(

        ))
      ))
    )),
    Branch("M2", List(
      Choice("B", "C", List(
        Branch("M3", List(
          
        )),
        Branch("M4", List(

        ))
      ))
    ))
  ))
)

val testConcat = List(
  Choice("A", "B", List(
    Branch("M1", List(
      Interaction("B", "C", "M3")
    )),
    Branch("M2", List(
    ))
  )),
  Interaction("B", "C", "M4")
)

val testConcat2 = List(
  Choice("A", "B", List(
    Branch("M1", List(
      Interaction("B", "C", "M3"),
      Interaction("B", "C", "M4")
    )),
    Branch("M2", List(
      Interaction("B", "C", "M4")
    ))
  ))
)

val testParOk = List (
  Parallel(
    List(Interaction("A", "B", "M0")),
    List(Interaction("C", "D", "M0"))
  )
)

val testParBad = List (
  Parallel(
    List(Interaction("A", "B", "M0")),
    List(Interaction("B", "C", "M0"))
  )
)

def error(msg: String) = throw new IllegalArgumentException(msg)

def desugar(interactions: Global): Global = interactions map { interaction =>
  interaction match {
    case Interaction(fromRole, toRole, msgSignature) =>
      Choice(fromRole, toRole, Branch(msgSignature, Nil)::Nil)
    case Choice(from, to, branches) => Choice(from, to, branches map (b => Branch(b.label, desugar(b.body))))
    case Recursion(x, body) => Recursion(x, desugar(body))
    case Parallel(p1, p2) => Parallel(desugar(p1), desugar(p2))
    case l: RecursionLabel => l
  }
}

def sugar(local: Local): Local = local flatMap { l => l match {
  case SendChoice(to, List(branch)) => Send(to, branch.label)::sugar(branch.body)
  case SendChoice(to, branches) => SendChoice(to, branches map (b => b.copy(body = sugar(b.body))))::Nil
  case ReceiveChoice(from, List(branch)) => Receive(from, branch.label)::sugar(branch.body)
  case ReceiveChoice(from, branches) => ReceiveChoice(from, branches map (b => b.copy(body = sugar(b.body))))::Nil
  case LocalRecursion(x, body) => LocalRecursion(x, sugar(body))::Nil
  case l: RecursionLabel => l::Nil
}}

import PartialFunction.cond
def contains(global: Global, role: String): Boolean = global exists (cond(_) {
  case Interaction(from, to, _) => from == role || to == role
  case Choice(from, to, branches) => 
    from == role || to == role || (branches exists {b => contains(b.body, role)})
  case Recursion(x, body) => contains(body, role)
  case RecursionLabel(_) => false
  case Parallel(p1, p2) => contains(p1, role) || contains(p2, role)
})
  
object Projector {
  def sugarProject(global: Global, projRole: String): Local = 
    sugar(project(desugar(global), projRole))

  def project(global: Global, projRole: String): Local = global match {
    case Nil => Nil
    case head::tail =>
      try {
        (head match {
          case Choice(`projRole`, toRole, branches) =>
            SendChoice(toRole, projectBranches(branches, projRole))::Nil
          case Choice(fromRole, `projRole`, branches) =>
            ReceiveChoice(fromRole, projectBranches(branches, projRole))::Nil
          case Choice(fromRole, toRole, branches) => 
            mergeAll(projectBranchBodies(branches, projRole), !tail.isEmpty)

          case Recursion(x, contents) => 
            LocalRecursion(x, project(contents, projRole))::Nil
          case RecursionLabel(x) => RecursionLabel(x)::Nil
          case Parallel(p1, p2) if contains(p1, projRole) && !contains(p2, projRole) => project(p1, projRole)
          case Parallel(p1, p2) if contains(p2, projRole) && !contains(p1, projRole) => project(p2, projRole)
          case Parallel(p1, p2) if !contains(p1, projRole) && !contains(p2, projRole) => Nil:Local

          case x => error("not projectable: " + x)
        }) ::: project(tail, projRole)
      } catch {
        case c:ConcatNeeded =>
          val choice = head.asInstanceOf[Choice]
          project(choice.concat(tail)::Nil, projRole)
      }
  }

  def merge(local1: Local, local2: Local, canConcat: Boolean): Local = (local1, local2) match {
    case ((c1: ReceiveChoice)::rest1, (c2: ReceiveChoice)::rest2) => 
      mergeChoices(c1.concat(rest1), c2.concat(rest2))::Nil

    case (LocalRecursion(x1, body1)::rest1, LocalRecursion(x2, body2)::rest2) if x1 == x2 => 
      // a first approximation. this still won't merge rec X {T} and
      // T ; rec X {T}. More work needed on this, termination is tricky
      LocalRecursion(x1, merge(body1, body2, canConcat))::merge(rest1, rest2, canConcat)
      
    case ((s1: SendChoice)::rest1, (s2: SendChoice)::rest2) if s1 == s2 =>
      s1::merge(rest1, rest2, canConcat)
    case (RecursionLabel(x)::rest1, RecursionLabel(y)::rest2) if x == y =>
      RecursionLabel(x)::merge(rest1, rest2, canConcat)

    case (_::_, Nil) | (Nil, _::_) if canConcat => throw new ConcatNeeded()

    case (Nil, Nil) => Nil
    case (a,b) => error("Not mergeable: "+a+" and "+b)
  }

  def mergeAll(toMerge: List[Local], canConcat: Boolean): Local = toMerge reduceLeft {
    (local, merged) => merge(local, merged, canConcat)
  }

  def projectBranches(branches: List[Branch], projRole: String): List[LocalBranch] =
    branches map { branch => LocalBranch(branch.label, project(branch.body, projRole)) }

  def projectBranchBodies(branches: List[Branch], projRole: String): List[Local] = 
    branches map { branch => project(branch.body, projRole) }

  def sameLabelBranch(c: ReceiveChoice, branch: LocalBranch): Option[LocalBranch] =
    c.branches find (b => b.label == branch.label)

  def mergeChoices(choice1: ReceiveChoice, choice2: ReceiveChoice) = {
    if (choice1.from != choice2.from) error("not mergeable")
    val mergedBranches = (choice1.branches foldLeft List[LocalBranch]()) { (merged, b) =>
      val b2 = sameLabelBranch(choice2, b)
      if (b2.isDefined) {
        if (b == b2.get) merged // skip b, added from choice2
        else error("not mergeable: " + b + " and " + b2.get)
      } else { // branch label not in choice2
        b :: merged
      }
    } ::: choice2.branches // skipped b added here, and all choice2 branches not in choice1
    ReceiveChoice(choice1.from, mergedBranches)
  }

  class ConcatNeeded extends scala.util.control.ControlThrowable
}

