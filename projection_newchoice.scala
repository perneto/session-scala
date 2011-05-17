object Projector {

  sealed trait Global
  sealed trait Local
  
  case class Message(from: String, to: String, msgSig: String) extends Global
  case class ChoiceScope(role: String, body: Global) extends Global
  case class Or(g1: Global, g2: Global) extends Global
  case class Recursion(label: String, body: Global) extends Global
  case class Parallel(g1: Global, g2: Global) extends Global
  case class Invitation(from: String, to: String) extends Global
  case class Seq(seq: List[Global]) extends Global
  def Seq(gs: Global*): Seq = Seq(List(gs: _*))
  case class RecursionLabel(label: String) extends Global with Local
  
  case class LocalOr(l1: Local, l2: Local) extends Local
  case class LocalRecursion(label: String, body: Local) extends Local
  case class Send(to: String, msgSig: String) extends Local
  case class Receive(from: String, msgSig: String) extends Local
  case class LocalSeq (seq: List[Local]) extends Local
  def LSeq(ls: List[Local]): LocalSeq = LocalSeq(ls filter (_ != Eps))
  def LSeq(local1: Local, local2: Local): LocalSeq = (local1, local2) match {
    case (LocalSeq(l1), LocalSeq(l2)) => LSeq(l1 ++ l2)
    case (LocalSeq(l1), l2) => LSeq(l1 :+ l2)
    case (l1, LocalSeq(l2)) => LSeq(l1 :: l2)
    case (l1, l2) => LSeq(List(l1, l2))
  }
  val Eps = LocalSeq(Nil)
  case class InviteSend(to: String) extends Local
  case class InviteReceive(from: String) extends Local
  
  def error(msg: String) = throw new IllegalArgumentException(msg)
  def notMergeable(a: Local, b: Local) = error("not mergeable: "+a+" and "+b)
  
  import PartialFunction.cond
  def contains(global: Global, role: String): Boolean = cond(global) {
    case Message(from, to, _) => from == role || to == role
    case ChoiceScope(at, body) => at == role || contains(body, role)
    case Recursion(x, body) => contains(body, role)
    case Parallel(g1, g2) => contains(g1, role) || contains(g2, role)
    case Or(g1, g2) => contains(g1, role) || contains(g2, role)
    case Seq(gs) => gs exists (contains(_, role))
  }
  
  def heads(global: Global): Set[Message] = global match {
    case i: Message => Set(i)
    case ChoiceScope(at, body) => heads(body)
    case Recursion(x, body) => heads(body)
    case RecursionLabel(_) | Invitation(_,_)  => Set.empty
    case Parallel(g1, g2) => Set.empty // cannot count contents as heads as order is non-deterministic
    case Or(g1, g2) => heads(g1) union heads(g2)
    case Seq(gs) => 
      if (gs.isEmpty) Set.empty
      else gs.head match {
        case Invitation(_,_) => heads(Seq(gs.tail))
        case x => heads(x)
      }
  }
  
  def freeRoles(g: Global): Set[String] = g match {
    case Message(from,to,msgSig) => Set(from, to)
    case ChoiceScope(role,g) => Set(role)
    case Or(g1,g2) => freeRoles(g1) union freeRoles(g2)
    case Recursion(x,g) => freeRoles(g)
    case RecursionLabel(x) => Set.empty
    case Invitation(from,to) => Set(from,to)
    case Parallel(g1,g2) => freeRoles(g1) union freeRoles(g2)
    case Seq(l) => (l foldLeft Set.empty[String])(_ union freeRoles(_))
  }
  
  def invitedRoles(g: Global): Set[String] = g match {
    case Invitation(_,to) => Set(to)
    case ChoiceScope(_,g) => invitedRoles(g)
    case Recursion(_,g) => invitedRoles(g)
    case Parallel(g1,g2) => invitedRoles(g1) union invitedRoles(g2)
    case Or(g1,g2) => invitedRoles(g1) union invitedRoles(g2)
    case Seq(gs) => (gs foldLeft Set.empty[String])(_ union invitedRoles(_))
    case _ => Set.empty
  }

  def project(global: Global, projRole: String): Local = project(global, projRole, None)
  
  def project(global: Global, projRole: String, choiceRole: Option[String]): Local = global match {
    case Message(`projRole`, to, sig) => Send(to, sig)
    case Message(from, `projRole`, sig) => Receive(from, sig)
    case Message(_,_,_) => Eps
    
    case ChoiceScope(`projRole`, body) => project(body, projRole, Some(projRole))
    case ChoiceScope(fromRole, body) => project(body, projRole) 
      
    case Or(g1,g2) => 
      val l1 = project(g1, projRole, choiceRole); val l2 = project(g2, projRole, choiceRole)
      val inv1 = invitedRoles(g1); val inv2 = invitedRoles(g2)
      if (l1 == Eps && l2 == Eps) Eps
      else if (inv1 contains projRole) l1
      else if (inv2 contains projRole) l2
      else if (choiceRole.isEmpty) merge(l1, l2)
      else if (choiceRole.get == projRole) LocalOr(l1, l2)
      else error("not projectable: "+global)
      
    case Recursion(x, contents) => 
      if (freeRoles(contents) contains projRole)
        LocalRecursion(x, project(contents, projRole))
      else
        Eps
      
    case label: RecursionLabel => label

    case Parallel(p1, p2) if contains(p1, projRole) && !contains(p2, projRole) => project(p1, projRole)
    case Parallel(p1, p2) if contains(p2, projRole) && !contains(p1, projRole) => project(p2, projRole)
    case Parallel(p1, p2) if !contains(p1, projRole) && !contains(p2, projRole) => Eps

    case Invitation(from, to) => 
      if (from == projRole) InviteSend(to)
      else if (to == projRole) InviteReceive(from)
      else Eps

    case Seq(gs) => LSeq(gs map (g => project(g, projRole, choiceRole)))
      
    case x => error("not projectable: " + x)
  }
  
  def seqSimple(seq: Local, l: List[Local], simple: Local) = {
    if (l.isEmpty) notMergeable(seq, simple)
    val h = l.head
    merge(h,simple) match {
      case LocalOr(`h`, `simple`) => LocalOr(seq, simple)
      case _ => notMergeable(seq, simple)
    }      
  }

  def orSimple(or: Local, l1: Local, l2: Local, simple: Local) = {
    merge(l1,simple) match {
      case LocalOr(`l1`, `simple`) => LocalOr(or, simple)
      case merged => LocalOr(merged, l2)
    }
  }

  val mergeBody: PartialFunction[(Local,Local), Local] = {
    case (l1, l2) if l1 == l2 => l1

    case (r1@Receive(from1, sig1), r2@Receive(from2, sig2)) if from1 != from2 || sig1 != sig2 =>
      LocalOr(r1, r2)

    case (rec@LocalRecursion(x1, body1), local2) =>
      merge(body1,local2) match {
        case LocalOr(`body1`,`local2`) => LocalOr(LocalRecursion(x1, body1), local2)
        case _ => notMergeable(rec, local2)
      }

    case (seq1@LocalSeq(list1), seq2@LocalSeq(list2)) =>
      if (list1.isEmpty || list2.isEmpty) notMergeable(seq1, seq2) // (Eps, Eps) handled by case (l1, l2) above
      val head1 = list1.head; val head2 = list2.head
      merge(head1, head2) match {
        case LocalOr(`head1`,`head2`) => LocalOr(seq1, seq2)
        case merged => LSeq(merged, merge(LSeq(list1.tail), LSeq(list2.tail)))
      }

    case (seq@LocalSeq(list1), or@LocalOr(l1, l2)) =>
      val head1 = list1.head
      merge(head1, l1) match {
        case LocalOr(`head1`, `l1`) => LocalOr(seq, or)
        case merged => LocalOr(LSeq(merged :: list1.tail), l2)
      }

    case (seq1@LocalSeq(l), r2: Receive) => seqSimple(seq1, l, r2)
    case (seq1@LocalSeq(l),  s2: Send) => seqSimple(seq1, l, s2)
      
    case (or@LocalOr(l1,l2), r: Receive) => orSimple(or, l1, l2, r)
    case (or@LocalOr(l1,l2), s: Send) => orSimple(or, l1, l2, s)

    case (o1@LocalOr(l1,l2), o2@LocalOr(l3,l4)) =>
      merge(
        merge(merge(l1,o2), merge(l2,o2)), 
        merge(merge(l3,o1), merge(l4,o1))
      )
  }
  
  def merge(l1: Local, l2: Local): Local = {
    if (mergeBody.isDefinedAt((l1, l2))) mergeBody((l1, l2))
    else if (mergeBody.isDefinedAt((l2, l1))) mergeBody(l2, l1)
    else notMergeable(l1,l2)
  }
  
  /*
  def mergeAll(toMerge: List[Local]): Local = toMerge reduceLeft {
    (local, merged) => merge(local, merged)
  }
  */

  def pretty(local: Local) {
    local match {
      case Eps =>
      case LocalSeq(l) =>
        println("{")
        l foreach pretty
        print("}")
      case LocalOr(l1, l2) =>
        pretty(l1)
        print(" + ")
        pretty(l2)
      case LocalRecursion(x, l) =>
        print("rec "+x+" ")
        pretty(l)
      case x => println(x)
    }
  }

  val testNotMergeable = 
    ChoiceScope("A", Seq(
      Or(
        Message("A","B","Int"),
        Message("A","C","Int")
      )
    ))
  
  val testMerge = 
      ChoiceScope("A", Seq(
        Or(
          Seq(
            Message("A","B","M1"),
            Message("B","C","M1")
          ),
          Or(
            Seq(
              Message("A","B","M2"),
              Message("B","C","M2")
            ),
            Seq(
              Message("A","B","M3"),
              Message("B","C","M2")
            )
          )
        )
      ))
  
  val testCliMidServ = Seq(
    Message("Cli", "Mid", "request"),
    ChoiceScope("Mid",
      Or(
        Seq(
          Invitation("Mid", "Serv"),
          Message("Mid", "Serv", "subreq"),
          Message("Serv", "Cli", "reply1")
        ),
        Message("Mid", "Cli", "reply2")
      )
    )
  )
  
  val testPrefix = Seq(
    ChoiceScope("A", 
      Or(
        Seq(
          Message("A", "C", "M0"),
          Message("A", "B", "M1")
        ),
        Seq(
          Message("A", "C", "M0"),
          Message("A", "B", "M2")
        )
      )
    )
  )
  
  val testSuffix = Seq(
    ChoiceScope("A", 
      Or(
        Seq(
          Message("A", "B", "M1"),
          Message("A", "C", "M0")          
        ),
        Seq(
          Message("A", "B", "M2"),
          Message("A", "C", "M0")
        )
      )
    )
  )
  
  val testRecur = Seq(
    ChoiceScope("A",
      Or(
        Recursion("x", Seq(
          Message("A", "B", "M1"),
          RecursionLabel("x")
        )),
        Message("A", "B", "M2")
      )
    )
  )
}
