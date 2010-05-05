package uk.ac.ic.doc.sessionscala

import actors.Actor

case class NewAccept(role: String, srcActor: Actor)

private[sessionscala] class AcceptState(awaiting: Set[String], s: State ) {

  def this(awaiting: Set[String]) = this(awaiting, Map())

  def roles = s.map({case (role, _) => role}).toSet
  def isComplete = roles == awaiting

  def received(role: String, srcActor: Actor, sender: OC) = {
    //println("received role: " + role + " srcActor: " + srcActor + ", sender: " + sender)
    val newList = (srcActor, sender) :: {
      if (s.contains(role)) s(role) else Nil
    }
    val newS = s.updated(role, newList)
    new AcceptState(awaiting, newS)
  }

  def dropHead: State = {
    s.foldLeft(Map():State) {
      case (m, (role, list)) =>
        val l = list drop 1
        if (l.isEmpty) m - role
        else m.updated(role, l)
    }
  }

  def createSessionChanAndReply = {
    import scala.collection.mutable.{Map => MMap}
    val mapping: MMap[String, ParticipantChannel] = MMap()
    val pairs = s map {
      // Incomplete pattern because we know the list has at least one element
      // (precondition to call this method: isComplete == true)
      case (role, (srcActor, sender)::xs) =>
        val chanToActor = new ParticipantChannel(srcActor)
        val sessChan = new SessionChannelImpl(mapping, chanToActor)
        mapping += (role -> chanToActor)
        (sender, sessChan)
    }
    pairs foreach { case (sender, sessChan) =>
        sender ! sessChan
    }
    new AcceptState(awaiting, dropHead)
  }
}