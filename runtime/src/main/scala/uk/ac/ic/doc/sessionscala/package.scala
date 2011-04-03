package uk.ac.ic.doc

import actors.{DaemonActor, Actor, OutputChannel}

package object sessionscala {
  /*private [sessionscala]*/ type OC = OutputChannel[Any]
  /*private [sessionscala]*/ type State = Map[Symbol, List[(Actor, OC)]]

  def daemonactor(_act: => Unit) = {
    val da = new DaemonActor { def act() = _act}
    da.start()
    da
  }
}