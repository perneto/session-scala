package uk.ac.ic.doc.sessionscala

import scala.actors.Actor._
import scala.actors._

object sessionops {
  def createLocalChannel(awaiting: Set[String]): SharedChannel = {
    if (awaiting.isEmpty) throw new IllegalArgumentException("At least one role is required")
    new SharedChannelSameVM(awaiting)
  }
  //def createAMQPChannel() : SharedChannel = new AMQPSharedChannel
}

trait SessionChannel extends (String => Channel[Any]) with InputChannel[Any]
private[sessionscala] class SessionChannelImpl(map: (String => Channel[Any]), var channel: Channel[Any])
        extends SessionChannel
{
  def apply(role: String) = map.apply(role)

  def ? = channel.?

  def reactWithin(msec: Long)(f: PartialFunction[Any, Unit]) = channel.reactWithin(msec)(f)

  def react(f: PartialFunction[Any, Unit]) = channel.react(f)

  def receiveWithin[R](msec: Long)(f: PartialFunction[Any, R]) = channel.receiveWithin(msec)(f)

  def receive[R](f: PartialFunction[Any, R]) = channel.receive(f)

  override def toString = "SessionChannelImpl{map:"+map+", channel: "+channel+"}"
}

trait SharedChannel {
  type ActorFun = SessionChannel => Unit
  def accept(role: String)(act: ActorFun): Unit
}

case class NewAccept(role: String, srcActor: Actor)

class AcceptState(awaiting: Set[String],
                  s: Map[String, List[(Actor, OutputChannel[Any])]] ) {
  type OC = OutputChannel[Any]
  type State = Map[String, List[(Actor, OutputChannel[Any])]]
  
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
    val mapping: MMap[String, Channel[Any]] = MMap()
    val pairs = s map {
      // Incomplete pattern because we know the list has at least one element
      // (precondition to call this method: isComplete == true)
      case (role, (srcActor, sender)::xs) =>
        val chanToActor = new Channel[Any](srcActor)
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

class SharedChannelSameVM(awaiting: Set[String]) extends SharedChannel {

  val coordinator = new DaemonActor {
    def act = {
      // This was tail-recursive before, but scalac won't optimize it.
      var s = new AcceptState(awaiting)
      loop {
        react {
          case NewAccept(role: String, srcActor: Actor) =>
            val newS = s.received(role, srcActor, sender)
            if (newS.isComplete) {
              s = newS.createSessionChanAndReply
            } else {
              s = newS
            }
        }
      }
    }
  }
  coordinator.start

  def accept(role: String)(act: ActorFun): Unit = {
    if (!awaiting.contains(role)) throw new IllegalArgumentException
            ("Role:" + role + " not defined on channel, awaiting: " + awaiting)
    //println("accept, awaiting: " + awaiting + ", role: " + role)
    val sessChan = (coordinator !? NewAccept(role, Actor.self)).asInstanceOf[SessionChannel]
    act(sessChan)
  }

}

class AMQPSharedChannel(awaiting: Set[String]) extends SharedChannel {
  def accept(role: String)(act: ActorFun) = actor {}
}

/*
object SessionAcceptorRegistry {
  def register(role: String, act,  channel: SharedChannel) {
    actor {
      loop {
        channel.accept(role, act) // blocks until all participants have called openSession
      }
    }
  }
}
*/