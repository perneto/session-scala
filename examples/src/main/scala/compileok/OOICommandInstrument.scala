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


session CmdInstrument =
 roles user[=1] : int,
       instrument,
       instrument_Registry ,
       instrument_Agent

 global main =
  InterfaceReq(string) from user to instrument_Registry;

  InterfaceData(string) from instrument_Registry to user;

  StdAccess from user to instrument_Agent;

  choice from instrument_Agent to user {
    Accept.(
      loop:
        choice from user to instrument_Agent {
          More_commands.(
            Commands(string) from user to instrument_Agent;
            Commands(string) from instrument_Agent to instrument;
            Response(int) from instrument to instrument_Agent;
            Response(int) from instrument_Agent to user;
            #loop)
        | Quit.(Leave from instrument_Agent to instrument)}
      )
  | Reject(string).(
     Err from instrument_Agent to instrument
    )
  }



 */
object OOICommandInstrument {
  def main(args: Array[String]) {
    withAMQPChannel(Set('User, 'CI_Authority, 'Instrument, 'Instrument_Registry, 'Instrument_Agent)) { sharedChannel =>

      sharedChannel.invite("", 'Buyer -> localhost, 'Seller -> localhost)

      actor {
        sharedChannel.accept('Seller) { s =>
          val item = s('Buyer).?[String]
          s('Buyer) ! 2000
          s('Buyer).receive {
            case address: String =>
              val deliveryDate = s('Buyer).?[String]
              println("placing order: " + item + " " + address + " " + deliveryDate)
            case 'quit =>
          }
          println("*****************Seller: finished")
        }
        println("############## Seller: sharedChannel.accept exited")
      }

      sharedChannel.accept('Buyer) { s =>
        s('Seller) ! "Widget A"
        val quote = s('Seller).?[Int]
        if (quote < 1000) {
          s('Seller) ! "123 Penny Lane"
          s('Seller) ! "4/6/2011 10:00 UTC-7"
        } else {
          s('Seller) ! 'quit
        }
        println("*****************Buyer: finished")
      }
      println("############## Buyer: sharedChannel.accept exited")
    }
    println("$$$$$$$$$$$$$$$$closed shared channel")
  }
}