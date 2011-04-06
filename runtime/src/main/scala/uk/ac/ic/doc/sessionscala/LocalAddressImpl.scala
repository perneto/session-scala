package uk.ac.ic.doc.sessionscala

import actors._, Actor._
import java.util.concurrent.TimeoutException

case class LocalAddressImpl(protocol: String, role: Symbol) extends Address {
  case object Take
  case class Msg(m: Any)
  // this lets us play nicer with the actors scheduler than a java BlockingQueue
  // blocking calls mess up the scheduler, but not react/receive as they are handled
  // specially by the scheduler
  val queueActor = daemonactor {
    loop {
      react {
        case Take => 
          //println(this+" QueueActor: got Take")
          val takeSender = sender
          react {
            case Msg(m) =>
              //println(this+" QueueActor: replying: "+m)
              takeSender ! m
          }
      }
    }
  }

  def receive() = {
    //println("receive on: " + this)
    queueActor !? Take
  }
  
  def receiveWithin(msec: Int): Any = {
    val res = queueActor !? (msec, Take)
    res.getOrElse(throw new TimeoutException)
  }

  def send(msg: Any) {
    //println("send msg: "+msg+" to: "+this)
    queueActor ! Msg(msg)
  }

  def derived(name: String) = new LocalAddressImpl(protocol, role)

  def convert(mapping: Map[Symbol, PrivateAddress]): Map[Symbol, Actor] = {
    //println("got map: "+mapping)
    mapping map {
      // Using pattern matching to deconstruct ActorPrivateAddress fails (gives a MatchError)
      // probably compiler bug, try again next release of Scala
      // pp: ActorPrivateAddress gives MatchError as well
      case (role: Symbol, pp: PrivateAddress) => 
        (role, pp.asInstanceOf[ActorPrivateAddress].a.asInstanceOf[Actor])
    }
  }

  def bindWithin[T](msec: Int)(act: (SessionChannel) => T) = {
    bind(receiveWithin(msec), act)
  }
  def bind[T](act: SessionChannel => T): T = {
    bind(receive, act)
  }
  def bind[T](rcvInvite: => Any, act: SessionChannel => T): T = {
    //println("bind: "+this+", role: "+role)
    rcvInvite match {
      case Invite(inviterPort, protocol, role) =>
        assert(role == this.role)
        val c = new Channel[Any]()
        val replyPort = ActorPrivateAddress(c)
        inviterPort.send(AcceptedInvite(role, replyPort, ActorPrivateAddress(self)))
        c.receive {
          case mapping: Map[Symbol, PrivateAddress] =>
            val actorMap = convert(mapping)
            //println("before act for "+role+", sessMap: " + sessMap)
            act(new SessionChannel(role, actorMap))
        }
    }
  }

  case class ActorPrivateAddress(a: OutputChannel[Any] with InputChannel[Any]) extends PrivateAddress {
    def send(msg: Any) {
      //println("sending "+msg+" to "+a)
      a ! msg
    }
  }
  
  def daemonactor(_act: => Unit) = {
    val da = new DaemonActor { def act() = _act}
    da.start()
    da
  }
}
