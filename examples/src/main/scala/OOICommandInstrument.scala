import scala.actors.Actor, Actor._
import uk.ac.ic.doc.sessionscala.Port
import Port._

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
11 bind() {
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
    val protocol = """
      protocol Test {
        role User, CI_Authority, Instrument, Instrument_Registry, Instrument_Agent;
      }
    """
    val user = AMQPPort(protocol, 'User, "user")
    val authority = AMQPPort(protocol, 'CI_Authority, "authority")
    val instrument = AMQPPort(protocol, 'Instrument, "instrument")
    val reg = AMQPPort(protocol, 'Instrument_Registry, "reg")
    val agent = AMQPPort(protocol, 'Instrument_Agent, "agent")

    actor { startSession(user, authority, instrument, reg, agent) }
    
    actor {
      user.bind { s =>
        //s('Instrument_Registry) ! 'interfaceReq(Id("submarine"))
        //s('Instrument_Registry).?
        println("*************** User: finished")
      }
    }

    actor {
      authority.bind { s =>
        println("***************** CI_Authority: finished")
      }
    }

    actor {
      instrument.bind { s =>
        println("***************** Instrument: finished")
      }
    }

    actor {
      reg.bind { s =>
        println("***************** Instrument_Registry: finished")
      }
    }

    agent.bind { s =>
      println("***************** Instrument_Agent: finished")
    }
  }
}