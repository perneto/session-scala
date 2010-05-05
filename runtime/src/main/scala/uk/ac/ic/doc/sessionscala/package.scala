package uk.ac.ic.doc

import actors.{Actor, OutputChannel}

package object sessionscala {
  private [sessionscala] type OC = OutputChannel[Any]
  private [sessionscala] type State = Map[String, List[(Actor, OC)]]
}