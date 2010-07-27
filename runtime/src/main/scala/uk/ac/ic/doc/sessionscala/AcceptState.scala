package uk.ac.ic.doc.sessionscala

import actors.{Channel, Actor}

case class NewAccept(role: String, srcActor: Actor)

private[sessionscala] class AcceptState(awaiting: Set[String], s: State ) {

  def this(awaiting: Set[String]) = this(awaiting, Map())

  def roles = s.map({case (role, _) => role}).toSet
  def isComplete = roles == awaiting

  def received(role: String, actorForRole: Actor, sender: OC) = {
    //println("received role: " + role + " actorForRole: " + actorForRole + ", sender: " + sender)
    val newList = (actorForRole, sender) :: {
      if (s.contains(role)) s(role) else Nil
    }
    val newS = s.updated(role, newList)
    new AcceptState(awaiting, newS)
  }

  def dropHead: State = {
    s.foldLeft(Map(): State) {
      case (m, (role, list)) =>
        val l = list drop 1
        if (l.isEmpty) m - role
        else m.updated(role, l)
    }
  }

  import scala.collection.mutable.{Map => MMap}
  import scala.collection.{Map => IMap}
  def createSessionChanAndReply = {
    val mapChans = MMap[String, IMap[String, Channel[Any]]]()
    val pairs = s map {
      // Incomplete pattern because we know the list has at least one element
      // (precondition to call this method: isComplete == true)
      case (role, (actorForRole, sender)::_) =>
        val chanForMsgFrom = MMap[String, Channel[Any]]()
        s foreach { case (otherRole, (actorForOther, _)::_) =>
            if (role != otherRole) {
              val chanForMsgFromOther = new Channel[Any](actorForRole)
              chanForMsgFrom += otherRole -> chanForMsgFromOther
            }
        }
        mapChans += role -> chanForMsgFrom
        (role, sender)
    }
    notifySenders(pairs, mapChans)
    new AcceptState(awaiting, dropHead)
  }

  type MapChans = IMap[String, IMap[String, Channel[Any]]]

  def notifySenders(pairs: IMap[String, OC], mapChans: MapChans) {
    pairs foreach { case (role, sender) =>
      sender ! makeSessionChanFor(role, mapChans)
    }
  }

  def makeSessionChanFor(thisRole: String,
                         mapChans: MapChans): String => ParticipantChannel = {
    val mapping = MMap[String, ParticipantChannel]()

    mapChans foreach { case (role, chanForMsgFrom) =>
      if (role != thisRole) {
        val chanToRole = chanForMsgFrom(thisRole)
        val chanFromRole = mapChans(thisRole)(role)
        mapping += role -> new ParticipantChannel(chanFromRole, chanToRole)
      }
    }
    mapping
  }
}