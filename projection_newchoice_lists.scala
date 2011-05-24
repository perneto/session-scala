def all[A, S >: A](c: collection.TraversableLike[A, _])(predicate: S => Boolean) = 
  !c.exists((e:S) => !predicate(e))

object Projector {

  sealed trait Global
  sealed trait Local
  
  case class Message(from: String, to: String, msgSig: String) extends Global
  case class Or(choiceRole: String, gs: List[Global]) extends Global
  case class Recursion(label: String, body: Global) extends Global
  case class Parallel(g1: Global, g2: Global) extends Global
  case class Invitation(from: String, to: String, body: Global) extends Global
  case class Seq(seq: List[Global]) extends Global
  def Seq(gs: Global*): Seq = Seq(List(gs: _*))
  case class RecursionLabel(label: String) extends Global with Local
  
  case class LocalOr(ls: List[Local]) extends Local {
    require(!ls.isEmpty)
  }
  def LOr(ls: List[Local]) = if (ls.isEmpty) Eps else if (ls.length == 1) ls.head else LocalOr(ls)
  def LOr(local1: Local, local2: Local) = LocalOr(List(local1, local2))
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
  case class InviteSend(to: String, body: Local) extends Local
  case class InviteReceive(from: String, body: Local) extends Local
  
  def error(msg: String) = throw new IllegalArgumentException(msg)
  def notMergeable(a: Local, b: Local) = error("not mergeable: "+a+" and "+b)
  
  def heads(global: Global): Set[Global] = global match {
    case i: Message => Set(i)
    case Recursion(x, body) => heads(body)
    case label: RecursionLabel => Set(label)
    case Invitation(_,_,body)  => heads(body)
    case Parallel(g1, g2) => heads(g1) union heads(g2) // unnecessary for now but may be useful if we relax the Parallel rules
    case Or(_, gs) =>  (gs foldLeft Set[Global]())(_ union heads(_))
    case Seq(gs) => 
      if (gs.isEmpty) Set()
      else heads(gs.head)
  }
  
  def senders(g: Global) = heads(g) map (_.asInstanceOf[Message].from)
  
  def freeRoles(g: Global): Set[String] = g match {
    case Message(from,to,msgSig) => Set(from, to)
    case Or(choiceRole, gs) => (gs foldLeft Set(choiceRole))(_ union freeRoles(_))
    case Recursion(x,g) => freeRoles(g)
    case RecursionLabel(x) => Set.empty
    case Invitation(from,to,body) => Set(from,to) union freeRoles(body)
    case Parallel(g1,g2) => freeRoles(g1) union freeRoles(g2)
    case Seq(l) => (l foldLeft Set[String]())(_ union freeRoles(_))
  }
  def contains(global: Global, role: String) = freeRoles(global).contains(role)    
  
  def invites(g: Global): Set[(String,String)] = g match {
    case Invitation(from,to,body) => Set((from,to)) union invites(body)
    case Recursion(_,g) => invites(g)
    case Parallel(g1,g2) => invites(g1) union invites(g2)
    case Or(_,gs) => (gs foldLeft Set[(String,String)]())(_ union invites(_))
    case Seq(gs) => (gs foldLeft Set[(String,String)]())(_ union invites(_))
    case _ => Set.empty
  }
  def invitedRoles(g: Global) = invites(g) map (_._2)
  def inviters(g: Global) = invites(g) map (_._1)

  def wellformed(g: Global): Boolean = wellformed(g, None)
  def wellformed(global: Global, scopeChooser: Option[String]): Boolean = {
    def consistentHeads(g: Global, chooser: String): Boolean = {
      for (h <- heads(g)) {
        if (!h.isInstanceOf[Message]) return false
        val m = h.asInstanceOf[Message]
        if (m.from != chooser) return false
        for (h2 <- heads(g) if h != h2) {
          if (!h2.isInstanceOf[Message]) return false
          val m2 = h2.asInstanceOf[Message]
          if (m.from == m2.from && m.msgSig == m2.msgSig) return false
        }
      }
      true
    }
    
    global match {
      case Message(_,_,_) | RecursionLabel(_) | Eps => true
      case Invitation(_,_,body) => wellformed(body, scopeChooser)
      case Recursion(x,body) =>
        !heads(body).exists(_.isInstanceOf[RecursionLabel]) &&
                wellformed(body, scopeChooser)
      case Parallel(g1,g2) =>
        (freeRoles(g1) intersect freeRoles(g2)).isEmpty &&
                wellformed(g1, scopeChooser) && wellformed(g2, scopeChooser)

      case Seq(gs) => // non-empty since Eps handled above
        val g1 = gs.head
        val g1wf = wellformed(g1, scopeChooser)
        g1wf && (gs foldLeft Set[String]()){(inter,g) => inter intersect invitedRoles(g)}.isEmpty &&
                all(gs) { wellformed(_) }

      case Or(chooser,gs) =>
        (scopeChooser map (_ == chooser)).getOrElse(true) &&
        (for (g1 <- gs ; g2 <- gs if !(g1 eq g2)) 
          yield (invitedRoles(g1) intersect freeRoles(g2)).isEmpty).reduce(_ && _) &&
        all(gs) { g => (inviters(g) diff Set(chooser)).isEmpty } &&
        all(gs) { consistentHeads(_, chooser) } &&
        all(gs) { wellformed(_, Some(chooser)) }

    }
  }
  
  def project(global: Global, projRole: String): Local = global match {
    case Message(`projRole`, to, sig) => Send(to, sig)
    case Message(from, `projRole`, sig) => Receive(from, sig)
    case Message(_,_,_) => Eps
    
    case Or(choiceRole, gs) => 
      val projs = gs map (project(_, projRole))
      val invs = gs map invitedRoles
      val invited = (invs zipWithIndex) find { case (inv, i) => inv contains projRole }
      invited match {
        case Some((_, i:Int)) => projs(i)
        case None => if (choiceRole == projRole) LocalOr(projs)
                     else mergeAll(projs)
      }
      
    case Recursion(x, contents) => 
      if (freeRoles(contents) contains projRole)
        LocalRecursion(x, project(contents, projRole))
      else
        Eps
      
    case label: RecursionLabel => label

    case Parallel(p1, p2) if contains(p1, projRole) && !contains(p2, projRole) => project(p1, projRole)
    case Parallel(p1, p2) if contains(p2, projRole) && !contains(p1, projRole) => project(p2, projRole)
    case Parallel(p1, p2) if !contains(p1, projRole) && !contains(p2, projRole) => Eps

    case Invitation(from, to, body) => 
      val projBody = project(body, projRole)
      if (from == projRole) InviteSend(to, projBody)
      else if (to == projRole) InviteReceive(from, projBody)
      else projBody

    case Seq(gs) => LSeq(gs map (g => project(g, projRole)))
      
    case x => error("not projectable: " + x)
  }
  
  def mergeAll(toMerge: List[Local]): Local = toMerge reduce { merge(_, _) }
  
  def merge(l1: Local, l2: Local): Local = {
    
    def seqSimple(seq: Local, l: List[Local], simple: Local) = {
      if (l.isEmpty) notMergeable(seq, simple)
      val h = l.head
      merge(h,simple) match {
        case LocalOr(`h` :: `simple` :: Nil) => LocalOr(seq :: simple :: Nil)
        case _ => notMergeable(seq, simple)
      }      
    }
  
    def orSimple(or: Local, ls: List[Local], simple: Local) = {
      val (merging, notMerging) = ls partition {l => 
        merge(l,simple) match {
          case LocalOr(`l` :: `simple` :: Nil) => false
          case merged => true
        }
      }
      if (merging.isEmpty) LocalOr(simple :: ls)
      else {
        val m = merge(merging(0), simple)
        LocalOr(m :: notMerging)  
      }
    }
  
    val mergeBody: PartialFunction[(Local,Local), Local] = {
      case (l1, l2) if l1 == l2 => l1  
  
      case (r1@Receive(from1, sig1), r2@Receive(from2, sig2)) if from1 != from2 || sig1 != sig2 =>
        LOr(r1, r2)
  
      case (rec@LocalRecursion(x1, body1), local2) =>
        merge(body1,local2) match {
          case LocalOr(`body1` :: `local2` :: Nil) => LOr(LocalRecursion(x1, body1), local2)
          case _ => notMergeable(rec, local2)
        }
  
      case (seq1@LocalSeq(list1), seq2@LocalSeq(list2)) =>
        if (list1.isEmpty || list2.isEmpty) notMergeable(seq1, seq2) // (Eps, Eps) handled by case (l1, l2) above
        val head1 = list1.head; val head2 = list2.head
        merge(head1, head2) match {
          case LocalOr(`head1` :: `head2` :: Nil) => LOr(seq1, seq2)
          case merged => LSeq(merged, merge(LSeq(list1.tail), LSeq(list2.tail)))
        }
  
      case (seq@LocalSeq(lSeq), or@LocalOr(lOr)) =>
        if (lSeq.isEmpty) notMergeable(seq, or)
        assert(!lOr.isEmpty)
        val seqHead = lSeq.head
        val (merging, notMerging) = lOr partition { orBranch =>
          merge(seqHead, orBranch) match {
            case LocalOr(`seqHead` :: `orBranch` :: Nil) => false
            case merged => true
          }
        }
        if (merging.isEmpty)
          LocalOr(seq :: notMerging)
        else {
          val m = merge(merging(0), seqHead)
          LocalOr(LSeq(m :: lSeq.tail) :: notMerging)
        }
  
      case (seq1@LocalSeq(l), r2: Receive) => seqSimple(seq1, l, r2)
      case (seq1@LocalSeq(l),  s2: Send) => seqSimple(seq1, l, s2)
        
      case (or@LocalOr(ls), r: Receive) => orSimple(or, ls, r)
      case (or@LocalOr(ls), s: Send) => orSimple(or, ls, s)
  
      case (o1@LocalOr(ls1), o2@LocalOr(ls2)) =>
        merge(
          mergeAll(ls1 map (merge(_, o2))),
          mergeAll(ls2 map (merge(_, o1)))
        )
    }
  
    l1 match {
      case LocalSeq(List(single)) => 
        merge(single, l2)
      case _ => l2 match {
        case LocalSeq(List(single)) =>
          merge(l1, single)
        case _ =>
          if (mergeBody.isDefinedAt((l1, l2))) mergeBody((l1, l2))
          else if (mergeBody.isDefinedAt((l2, l1))) mergeBody(l2, l1)
          else notMergeable(l1,l2)
      }
    }
  }
  
  def pretty(local: Local) {
    local match {
      case Eps =>
      case LocalSeq(l) =>
        println("{")
        l foreach pretty
        print("}")
      case LocalOr(ls) =>
        pretty(ls.head) // ls is never empty
        ls.tail foreach {l =>
          print(" + ")        
          pretty(l)
        }
      case LocalRecursion(x, l) =>
        print("rec "+x+" ")
        pretty(l)
      case InviteSend(role,body) =>
        print("invitesend "+role+ " ")
        pretty(body)
      case InviteReceive(role,body) =>
        print("inviterecv "+role+ " ")
        pretty(body)
      case x => println(x)
    }
  }

  val testNotMergeable = 
    Or(
      "A",
      List(
        Message("A","B","Int"),
        Message("A","C","Int")
      )
    )
  
  val testMerge = 
    Or(
      "A",
      List(
        Seq(
          Message("A","B","M1"),
          Message("B","C","M1")
        ),
        Or(
          "A",
          List(
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
      )
    )
  
  val testMerge2 = 
    Or(
      "A",
      List(
      Seq(
        Message("A","B","M3"),
        Message("B","C","M2")
      ),
      Or(
        "A",
        List(
        Seq(
          Message("A","B","M2"),
          Message("B","C","M2")
        ),
        Seq(
          Message("A","B","M1"),
          Message("B","C","M1")  
        ))
      ))
    )
  
  val testMerge3 = 
    Or(
      "A",
      List(
        Seq(
          Message("A","B","M3"),
          Message("B","C","M2")
        ),
        
        Seq(
          Message("A","B","M2"),
          Message("B","C","M2")
        ),
        Seq(
          Message("A","B","M1"),
          Message("B","C","M1")  
        )
      )
    )
      
  val testCliMidServ = Seq(
    Message("Cli", "Mid", "request"),
    Or(
      "Mid",
      List(
      Invitation("Mid", "Serv", Seq(
        Message("Mid", "Serv", "subreq"),
        Message("Serv", "Cli", "reply1")
      )),
      Message("Mid", "Cli", "reply2")
      )
    )
  )
  
  val testPrefix =
    Or(
      "A",
      List(
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
  
  
  val testSuffix = 
    Or(
      "A",
      List(
      Seq(
        Message("A", "B", "M1"),
        Message("A", "C", "M0")          
      ),
      Seq(
        Message("A", "B", "M2"),
        Message("A", "C", "M0")
      ))
    )
    
  
  val testRecur =
    Or(
      "A",
      List(
      Recursion("x", Seq(
        Message("A", "B", "M1"),
        RecursionLabel("x")
      )),
      Message("A", "B", "M2")
      )
    )
   
  val testNotWF =
    Or(
      "A", List(
      Message("B","C","M1"),
      Message("A","B","M2"))
    )
}
