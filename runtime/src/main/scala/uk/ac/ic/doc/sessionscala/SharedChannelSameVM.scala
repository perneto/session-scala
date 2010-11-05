package uk.ac.ic.doc.sessionscala

import actors.{DaemonActor, Actor}

class SharedChannelSameVM(awaiting: Set[Symbol]) extends SharedChannel(awaiting) {

  val coordinator = new DaemonActor {
    def act = {
      // This was tail-recursive before, but scalac won't optimize it.
      var s = new AcceptState(awaiting)
      loop {
        react {
          case NewAccept(role: Symbol, actorForRole: Actor) =>
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

  def join(role: Symbol)(act: ActorFun): Unit = {
    checkRoleAwaiting(role)
    //println("join, awaiting: " + awaiting + ", role: " + role)
    val sessChan = (coordinator !? NewAccept(role, Actor.self)).asInstanceOf[Symbol => ParticipantChannel]
    act(sessChan)
  }

  def invite(mapping: (Symbol,String)*): Unit = {}
  def accept(role: Symbol)(act: ActorFun): Unit = {}

}
