package uk.ac.ic.doc

import actors.{Channel, Actor, OutputChannel}

package object sessionscala {
  /*private [sessionscala]*/ type OC = OutputChannel[Any]
  /*private [sessionscala]*/ type State = Map[Symbol, List[(Actor, OC)]]  
}