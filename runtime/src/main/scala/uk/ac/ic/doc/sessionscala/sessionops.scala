package uk.ac.ic.doc.sessionscala

import scala.actors.Actor._
import actors.{DaemonActor, OutputChannel, Actor}
import annotation.tailrec

object sessionops {
  def createLocalChannel(awaiting: Set[String]): SharedChannel = {
    if (awaiting.isEmpty) throw new IllegalArgumentException("At least one role is required")
    new SharedChannelSameVM(awaiting)
  }
  //def createAMQPChannel() : SharedChannel = new AMQPSharedChannel
}

trait SharedChannel {
  type ActorFun = (String => Actor) => Unit
  def accept(role: String)(act: ActorFun): Actor
}

case class NewAccept(role: String, act: SharedChannel#ActorFun)
case class Proceed(a: Actor)

class AcceptState(awaiting: Set[String],
                  s: Map[String, List[(SharedChannel#ActorFun, OutputChannel[Any])]] ) {
  type OC = OutputChannel[Any]
  
  def this(awaiting: Set[String]) = this(awaiting, Map())
  
  def roles = s.map({case (role, _) => role}).toSet
  def isComplete = roles == awaiting

  def received(role: String, act: SharedChannel#ActorFun, sender: OC) = {
    println("received: " + role + " " +act + " " + sender)
    val newList = (act, sender) :: {
      if (s.contains(role)) s(role) else Nil
    }
    val newS = s.updated(role, newList)
    new AcceptState(awaiting, newS)
  }

  def dropHead = {
    s.foldLeft(Map():Map[String, List[(SharedChannel#ActorFun, OC)]]) {
      case (m, (role, list)) =>
        m.updated(role, list drop 1)
    }
  }

  def createActorsAndReply = {
    import scala.collection.mutable.{Map => MMap}
    val mapping: MMap[String, Actor] = MMap()
    val pairs = s map {
      // Incomplete pattern because we know the list has at least one element
      // (precondition to call this method: isComplete == true)
      case (role, (actImpl, sender)::xs) =>
        val newActor = new Actor {def act = actImpl(mapping)}
        mapping += (role -> newActor)
        (newActor, sender)
    }
    pairs foreach {
      p =>
        p._1.start
        p._2 ! p._1
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
          case NewAccept(role: String, act: ActorFun) =>
            val newS = s.received(role, act, sender)
            if (newS.isComplete) {
              s = newS.createActorsAndReply
            } else {
              s = newS
            }
        }
      }
    }
  }
  coordinator.start

  def accept(role: String)(act: ActorFun): Actor = {
    if (!awaiting.contains(role)) throw new IllegalArgumentException
            ("Role:" + role + " not defined on channel, awaiting: " + awaiting)
    println("about to !?: " + this)
    (coordinator !? NewAccept(role, act)).asInstanceOf[Actor]
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