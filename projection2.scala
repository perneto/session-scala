trait GlobalInteraction
trait LocalInteraction

type Global = List[GlobalInteraction]
type Local = List[LocalInteraction]

case class Branch(label: String, body: Global)

case class Interaction(from: String, to: String, msgSig: String) extends GlobalInteraction
case class Choice(at: String, body: Global) extends GlobalInteraction 
case class Or(g1: Global, g2: Global) extends GlobalInteraction
case class Recursion(label: String, body: Global) extends GlobalInteraction 
case class RecursionLabel(label: String) extends GlobalInteraction with LocalInteraction
case class Parallel(g1: Global, g2: Global) extends GlobalInteraction

case class ReceiveChoice(from: String, body: Local) extends LocalInteraction
case class LocalOr(l1: Local, l2: Local) extends LocalInteraction
case class LocalRecursion(label: String, body: Local) extends LocalInteraction
case class Send(to: String, msgSig: String) extends LocalInteraction
case class Receive(from: String, msgSig: String) extends LocalInteraction

val testSimple = List(
  Choice("A", List(
    Or(
      Interaction("A","B","Int")::Nil,
      Interaction("A","C","Int")::Nil
    )
  ))
)

val testMerge = List(
    Choice("A", List(
      Or(
        List(
          Interaction("A","B","M1"),
          Interaction("B","C","M1")
        ),
        Or(
          List(
            Interaction("A","B","M2"),
            Interaction("B","C","M2")
          ),
          List(
            Interaction("A","B","M3"),
            Interaction("B","C","M2")
          )
        )::Nil
      )
    ))
)

def error(msg: String) = throw new IllegalArgumentException(msg)

import PartialFunction.cond
def contains(global: Global, role: String): Boolean = global exists (cond(_) {
  case Interaction(from, to, _) => from == role || to == role
  case Choice(at, body) => at == role || contains(body, role)
  case Recursion(x, body) => contains(body, role)
  case RecursionLabel(_) => false
  case Parallel(g1, g2) => contains(g1, role) || contains(g2, role)
  case Or(g1, g2) => contains(g1, role) || contains(g2, role)
})

def heads(global: Global): List[Interaction] = global flatMap { g => g match {
  case i: Interaction => List(i)
  case Choice(at, body) => heads(body)
  case Recursion(x, body) => heads(body)
  case RecursionLabel(_) => List.empty
  case Parallel(g1, g2) => List.empty // cannot count contents as heads as order is non-deterministic
  case Or(g1, g2) => heads(g1) union heads(g2)
}}

def hasDst(heads: List[Interaction], role: String): Boolean = heads exists (cond(_) {
  case Interaction(_, `role`, _) => true
})

def simplify(local: Local): Local = {
  def bind(fst: (Local, Boolean), snd: (Local, Boolean)): (Local, Boolean) =
    (fst._1 ::: snd._1, fst._2 || snd._2)
  def simplifyRec(local: Local): (Local, Boolean) = {
    local match { 
      case Nil => (Nil:Local, false)
      case l::ls => 
        val fst = l match {
          case LocalOr(Nil, other) => simplifyRec(other)
          case LocalOr(other, Nil) => simplifyRec(other)
          case ReceiveChoice(at, body) => 
            val (simple, hadOr) = simplifyRec(body)
            if (hadOr) (ReceiveChoice(at, simple)::Nil, true)
            else (simple, false)
          case LocalRecursion(x, body) => 
            val (simple, hadOr) = simplifyRec(body)
            (LocalRecursion(x, simple)::Nil, hadOr)
          case o@LocalOr(x::xs, y::ys) => (o::Nil, true)
          case other => (other::Nil, false)
        } 
        bind(fst, simplifyRec(ls))
    }
  }
  simplifyRec(local)._1
}
  
object Projector {
  def project(global: Global, projRole: String): Local = global match {
    case Nil => Nil
    case head::tail =>
      try {
        ((head match {
          case Interaction(`projRole`, to, sig) => Send(to, sig)::Nil
          case Interaction(from, `projRole`, sig) => Receive(from, sig)::Nil
          case Interaction(_,_,_) => Nil:Local
          case Choice(`projRole`, body) => project(body, projRole)
          case Choice(fromRole, body) if hasDst(heads(body), projRole) => 
            //ReceiveChoice(fromRole, project(body, projRole))::Nil
            project(body, projRole)
          case Choice(fromRole, body) => 
            project(body, projRole)
            //mergeTree(projectBranchBodies(branches, projRole), !tail.isEmpty)

          case Or(g1,g2) => LocalOr(project(g1,projRole), project(g2,projRole))::Nil

          case Recursion(x, contents) => 
            LocalRecursion(x, project(contents, projRole))::Nil
          case RecursionLabel(x) => RecursionLabel(x)::Nil

          case Parallel(p1, p2) if contains(p1, projRole) && !contains(p2, projRole) => project(p1, projRole)
          case Parallel(p1, p2) if contains(p2, projRole) && !contains(p1, projRole) => project(p2, projRole)
          case Parallel(p1, p2) if !contains(p1, projRole) && !contains(p2, projRole) => Nil:Local

          case x => error("not projectable: " + x)
        }): Local) ::: project(tail, projRole)
      } catch {
        case x => throw x
        /*
        case c:ConcatNeeded =>
          val choice = head.asInstanceOf[Choice]
          project(choice.concat(tail)::Nil, projRole)
        */
      }
  }

  /*
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
  */

  class ConcatNeeded extends scala.util.control.ControlThrowable
}

