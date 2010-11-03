package uk.ac.ic.doc

import actors.{Actor, OutputChannel}
import sessionscala.ParticipantChannel

package object sessionscala {
  type SessionChannel = (Symbol => ParticipantChannel)
  /*private [sessionscala]*/ type OC = OutputChannel[Any]
  /*private [sessionscala]*/ type State = Map[Symbol, List[(Actor, OC)]]
}