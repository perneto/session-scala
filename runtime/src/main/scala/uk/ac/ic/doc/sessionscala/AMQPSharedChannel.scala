package uk.ac.ic.doc.sessionscala


class AMQPSharedChannel(awaiting: Set[String]) extends SharedChannel {
  def accept(role: String)(act: ActorFun): Unit = { throw new IllegalStateException("TODO") }
}