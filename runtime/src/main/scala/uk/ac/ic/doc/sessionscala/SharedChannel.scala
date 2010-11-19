package uk.ac.ic.doc.sessionscala

object SharedChannel {
  def createLocalChannel(awaiting: Set[Symbol]): SharedChannel = {
    if (awaiting.isEmpty) throw new IllegalArgumentException("At least one role is required")
    new SharedChannelSameVM(awaiting)
  }
  def createAMQPChannel(awaiting: Set[Symbol], protocolFile: String, brokerHost: String = "localhost",
                        port: Int = 5672, user: String = "guest", password: String = "guest"): SharedChannel =
    new AMQPSharedChannel(awaiting, protocolFile, brokerHost, port, user, password)

  def localhost: String = java.net.InetAddress.getLocalHost.getCanonicalHostName

  def withAMQPChannel[T](awaiting: Set[Symbol], protocolFile: String = "", brokerHost: String = "localhost", 
                         port: Int = 5672, user: String = "guest", 
                         password: String = "guest")(block: SharedChannel => T): T = {
    val shared = createAMQPChannel(awaiting, protocolFile, brokerHost, port, user, password)
    try { block(shared) } finally { shared.close() }
  }
}

abstract class SharedChannel(val awaiting: Set[Symbol]) {
  type ActorFun = (Symbol => ParticipantChannel) => Unit
  /** Blocks until all awaited roles have joined. */
  def join(role: Symbol)(act: ActorFun): Unit

  /** Sends out invites. Needs to give a mapping for each awaited role.
      @param mapping Maps role names to host names or IP addresses. */
  def invite(mapping: (Symbol,String)*): Unit

  def forwardInvite(mapping: (Symbol,String)*): Unit
  /** Accept to play a given role. Waits for an invite before proceeding. */
  def accept(role: Symbol)(act: ActorFun): Unit

  def close() {}

  def checkRoleAwaiting(role: Symbol) {
    if (!awaiting.contains(role)) throw new IllegalArgumentException
            ("Role:" + role + " not defined on channel, awaiting: " + awaiting)
  }
}


/*
object SessionAcceptorRegistry {
  def register(role: String, act,  channel: SharedChannel) {
    actor {
      loop {
        channel.join(role, act) // blocks until all participants have called openSession
      }
    }
  }
}
*/
