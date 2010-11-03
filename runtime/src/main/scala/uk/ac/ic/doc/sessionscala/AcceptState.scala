package uk.ac.ic.doc.sessionscala

import actors.{Channel, Actor}

case class NewAccept(role: Symbol, srcActor: Actor)

private[sessionscala] class AcceptState(awaiting: Set[Symbol], s: State) {

  def this(awaiting: Set[Symbol]) = this(awaiting, Map())

  def roles = s.map({case (role, _) => role}).toSet
  def isComplete = roles == awaiting

  def received(role: Symbol, actorForRole: Actor, sender: OC) = {
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
    val mapChans = MMap[Symbol, IMap[Symbol, Channel[Any]]]()
    val pairs = s map {
      // Incomplete pattern because we know the list has at least one element
      // (precondition to call this method: isComplete == true)
      case (role, (actorForRole, sender)::_) =>
        val chanForMsgFrom = MMap[Symbol, Channel[Any]]()
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

  type MapChans = IMap[Symbol, IMap[Symbol, Channel[Any]]]

  def notifySenders(pairs: IMap[Symbol, OC], mapChans: MapChans) {
    pairs foreach { case (role, sender) =>
      sender ! makeSessionChanFor(role, mapChans)
    }
  }

  def makeSessionChanFor(thisRole: Symbol,
                         mapChans: MapChans): Symbol => ParticipantChannel = {
    val mapping = MMap[Symbol, ParticipantChannel]()

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