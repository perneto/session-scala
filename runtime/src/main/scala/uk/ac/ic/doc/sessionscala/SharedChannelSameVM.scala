package uk.ac.ic.doc.sessionscala

import actors.{DaemonActor, Actor}

class SharedChannelSameVM(awaiting: Set[String]) extends SharedChannel {

  val coordinator = new DaemonActor {
    def act = {
      // This was tail-recursive before, but scalac won't optimize it.
      var s = new AcceptState(awaiting)
      loop {
        react {
          case NewAccept(role: String, actorForRole: Actor) =>
            val newS = s.received(role, actorForRole, sender)
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

  def join(role: String)(act: ActorFun): Unit = {
    if (!awaiting.contains(role)) throw new IllegalArgumentException
            ("Role:" + role + " not defined on channel, awaiting: " + awaiting)
    //println("join, awaiting: " + awaiting + ", role: " + role)
    val sessChan = (coordinator !? NewAccept(role, Actor.self)).asInstanceOf[String => ParticipantChannel]
    act(sessChan)
  }

}