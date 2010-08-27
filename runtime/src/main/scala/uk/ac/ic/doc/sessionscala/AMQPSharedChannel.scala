package uk.ac.ic.doc.sessionscala


class AMQPSharedChannel(awaiting: Set[String]) extends SharedChannel {
  def join(role: String)(act: ActorFun): Unit = { throw new IllegalStateException("TODO") }
}