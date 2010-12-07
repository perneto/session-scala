package compileok

/**
 * Created by: omp08
 */

import scala.actors.Actor, Actor._
import uk.ac.ic.doc.sessionscala.SharedChannel
import SharedChannel._

/*
2 protocol OOI_Command_Instrument {
3 role User, CI_Authority, Instrument;
4 role Instrument_Registry, Instrument_Agent;
5
6 interfaceReq(Id) from User to Instrument_Registry;
7 interfaceData(Id) from Instrument_Registry to User;
8
9 stdAccess() from User to Instrument_Agent;
10 choice from Instrument_Agent to User {
11 accept() {
12 X: {
13 choice from User to Instrument_Agent {
14 more_commands() {
15 commands(List) from User to Instrument_Agent;
16 commands(List) from Instrument_Agent to Instrument;
17 response(Data) from Instrument to Instrument_Agent;
18 response(Data) from Instrument_Agent to User;
19 #X;
20 quit() { }
21 }
22 }
23 reject() {
24 errDetails(Data) from Instrument_Agent to User;
25 }
26 }
27 }

 */
case class Id(name: String)
class Data
class ListCommands

object OOICommandInstrument {
  def main(args: Array[String]) {
    withAMQPChannel(Set('User, 'CI_Authority, 'Instrument, 'Instrument_Registry, 'Instrument_Agent)) { sharedChannel =>

      sharedChannel.invite("", 'User -> localhost, 'CI_Authority -> localhost, 'Instrument -> localhost, 'Instrument_Registry -> localhost, 'Instrument_Agent -> localhost)

      actor {
        sharedChannel.accept('User) { s =>
          //s('Instrument_Registry) ! 'interfaceReq(Id("submarine"))
          //s('Instrument_Registry).?
          println("*************** User: finished")
        }
      }

      actor {
        sharedChannel.accept('CI_Authority) { s =>
          println("***************** CI_Authority: finished")
        }
      }

      actor {
        sharedChannel.accept('Instrument) { s =>
          println("***************** Instrument: finished")
        }
      }

      actor {
        sharedChannel.accept('Instrument_Registry) { s =>
          println("***************** Instrument_Registry: finished")
        }
      }

      //actor {
        sharedChannel.accept('Instrument_Agent) { s =>
          println("***************** Instrument_Agent: finished")
        }
      //}

    }
  }
}