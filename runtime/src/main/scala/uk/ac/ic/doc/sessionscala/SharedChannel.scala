package uk.ac.ic.doc.sessionscala

import scala.actors._

object SharedChannel {
  def createLocalChannel(awaiting: Set[String]): SharedChannel = {
    if (awaiting.isEmpty) throw new IllegalArgumentException("At least one role is required")
    new SharedChannelSameVM(awaiting)
  }
  def createAMQPChannel(awaiting: Set[String]) : SharedChannel = new AMQPSharedChannel(awaiting)
}

trait SharedChannel {
  type ActorFun = SessionChannel => Unit
  def accept(role: String)(act: ActorFun): Unit
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