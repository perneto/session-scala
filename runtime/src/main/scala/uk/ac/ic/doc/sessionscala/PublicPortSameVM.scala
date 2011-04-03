package uk.ac.ic.doc.sessionscala

import actors.{Actor, Channel, OutputChannel}, Actor._

class PublicPortSameVM(val protocol: String, val role: Symbol)
        extends PublicPort {
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

  def send(msg: Any) {
    //println("send msg: "+msg+" to: "+this)
    queueActor ! Msg(msg)
  }

  def derived(name: String) = new PublicPortSameVM(protocol, role)

  def convert(mapping: Map[Symbol, PrivatePort]): Map[Symbol, Actor] = {
    //println("got map: "+mapping)
    mapping map {
      // Using pattern matching to deconstruct ActorPrivatePort fails (gives a MatchError)
      // probably compiler bug, try again next release of Scala
      case (role: Symbol, pp: PrivatePort) => 
        (role, pp.asInstanceOf[ActorPrivatePort].a.asInstanceOf[Actor])
    }
  }

  /*
   * Used to wire up channels for communication between actors.
   *
   * A Channel is an anonymous tag associated with one actor instance, the receiver.
   * It allows the receiver to segregate between messages
   * on its actor mailbox, thus implementing a multiplexed channel abstraction.
   *
   * To establish two-way communication, each actor in the session needs
   * a mapping of roles to ChannelPair. Each channel pair has
   * a channel for messages from the other role (a tag for our mailbox),
   * and a channel for messages to the other role (a tag we add to messages we send to it).
   *
   * Building this mapping takes three steps:
   * - in each actor, create the Channels *to* each Actor *from* each of the others
   *   (hence the Channel's receiver is self)
   * - in each actor, send each other's channel to the corresponding Actor.
   * - receiving a message (role,channel) from each actor: add the passed Channel to the local mapping
   *   as the channel to use to contact the other actor's role.
   *
   *  Note that we create a channel for self-messaging, and we send it to ourselves. Not very useful,
   *  but it makes the code simpler and the user syntax gets more expressive for it.
   */
  def buildSessionMapping[T](actorMap: Map[Symbol, Actor], ourRole: Symbol): Map[Symbol, ChannelPair] = {
    val emptyMap = Map.empty[Symbol, (Actor, Channel[Any])]
    val chansToUs = actorMap map { case (otherRole, otherActor) =>
      val chanFromOtherToUs = new Channel[Any](Actor.self)
      (otherRole -> (otherActor, chanFromOtherToUs))
    }

    chansToUs map  { case (otherRole, (otherActor, chanFromOtherToUs)) =>
      otherActor ! (ourRole, chanFromOtherToUs)
      //println("At "+role+": sent channel "+chanFromOtherToUs+" to "+otherActor+" for "+otherRole)
    }
    //println("At "+role+": done sending channels to others")

    var chansToOthers = Map[Symbol, Channel[Any]]()
    while (chansToOthers.size < chansToUs.size) {
      //println("At "+role+": receiving, self:"+self+", mailboxSize: "+mailboxSize)
      self.receive {
        // Receive block and not simply ? because the other actors could be already started 
        // and sending us session messages.
        case (otherRole: Symbol, chanToOtherFromUs: Channel[Any]) =>  
          //println("At "+role+": received from "+otherRole+", chansToOthers: "+chansToOthers)
          chansToOthers += (otherRole -> chanToOtherFromUs)
      }
    }

    for ((otherRole, chanToOther) <- chansToOthers)
      yield (otherRole ->
             ChannelPair(chanToOther, chansToUs(otherRole)._2))
  }

  def bind[T](act: SessionChannel => T): T = {
    //println("bind: "+this+", role: "+role)
    receive() match {
      case Invite(inviterPort, protocol, role) =>
        assert(role == this.role)
        val c = new Channel[Any]()
        val replyPort = ActorPrivatePort(c)
        inviterPort.send(AcceptedInvite(role, replyPort, ActorPrivatePort(self)))
        c.receive {
          case mapping: Map[Symbol, PrivatePort] =>
            val actorMap = convert(mapping)
            val sessMap = buildSessionMapping(actorMap, role)
            //println("before act for "+role+", sessChan: " + sessMap)
            act(new SessionChannel(role, sessMap))
        }
    }
  }

  case class ActorPrivatePort(a: OutputChannel[Any]) extends PrivatePort {
    def send(msg: Any) {
      //println("sending "+msg+" to "+a)
      a ! msg
    }
  }
}
