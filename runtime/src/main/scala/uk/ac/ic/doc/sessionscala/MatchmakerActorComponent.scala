package uk.ac.ic.doc.sessionscala

import collection.mutable
    
/**
 * Created by: omp08
 */

trait MatchmakerActorComponent {

  case class Invite(role: Symbol, sessExchange: String, protocol: String)
  case class Accept(role: Symbol)
  case object Exit

  import actors.Actor._
  val matchMakerActor = actor {
    println("started matchmaker")

    val invites = mutable.Map.empty[Symbol, List[(String,String)]] // List[(ExchangeName, Protocol)]
    val accepts = mutable.Map.empty[Symbol, List[OC]]

    def matchMsg[T1,T2](map: mutable.Map[Symbol, List[T1]], key: Symbol, value: T2, otherMap: mutable.Map[Symbol, List[T2]])(action: T1 => Unit) {
      map get key match {
        case Some(x :: xs) =>
          action(x)
          if (xs == Nil) map -= key
          else map.update(key, xs)
        case _ =>
          map -= key
          otherMap.update(key, value :: otherMap.getOrElse(key, Nil))
      }
    }

    loop {
      react {
        case i: Invite =>
          println("matchmaker: got invite " + i)
          matchMsg(accepts, i.role, (i.sessExchange, i.protocol), invites) { acceptSender =>
            acceptSender ! (i.sessExchange, i.protocol)  //todo: give list of local proxies to proxy registry so it can give it to all proxies
          }
        case Accept(acceptRole: Symbol) =>
          println("matchmaker: got accept for role " + acceptRole)
          matchMsg(invites, acceptRole, sender, accepts) { sessExchAndProtocol =>
            sender ! sessExchAndProtocol // todo: ditto above
          }
        case Exit =>
          println("Matchmaker exiting")
          exit()
      }
    }
  }

}