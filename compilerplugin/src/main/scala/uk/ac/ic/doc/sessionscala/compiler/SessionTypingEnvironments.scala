package uk.ac.ic.doc.sessionscala.compiler

import tools.nsc.Global
import org.scribble.common.logging.Journal
import org.scribble.protocol.model.{Participant, ProtocolModel}
import org.scribble.protocol.projection.impl.ProtocolProjectorImpl

trait SessionTypingEnvironments {
  val scribbleJournal: Journal
  val global: Global
  import global._

  import collection.mutable.{Map => MMap}

  val projector = new ProtocolProjectorImpl()

  trait SessionTypingEnvironment {
    val sharedChannels: MMap[Name, ProtocolModel] = MMap()
    val parent: SessionTypingEnvironment

    def isSessionChannel(c: Name) = false

    def registerSharedChannel(name: Name, globalType: ProtocolModel) {
      println("Putting in map: " + name -> globalType)
      sharedChannels += name -> globalType
      println(sharedChannels)
    }

    def enterAccept(sharedChannel: Name, role: String, sessChan: Name): SessionTypingEnvironment = {
      println("enterAccept:" + sharedChannels)
      val participant = new Participant(role)
      val globalModel = getGlobalTypeForChannel(sharedChannel)
      val projectedModel = projector.project(globalModel, participant, scribbleJournal)
      println(projectedModel)
      return new InProcessEnvironment(this, participant, projectedModel, sessChan)
    }

    def leaveAccept : SessionTypingEnvironment = {
      throw new IllegalStateException("this should not happen")
    }

    def enterBranch(label: Type) : SessionTypingEnvironment = {
      throw new IllegalStateException("this should not happen")
    }

    def leaveBranch : SessionTypingEnvironment = {
      throw new IllegalStateException("this should not happen")
    }

    def getGlobalTypeForChannel(name: Name): ProtocolModel = {
      sharedChannels.get(name) match {
        case Some(p) => p
        case None =>
          if (parent == null)
            throw new IllegalArgumentException("Channel: " + name + " is not in scope")

          else
            parent.getGlobalTypeForChannel(name)
      }
    }

    def send(sessChan: Name, role: String, msgType: Type) {}
    def receive(sessChan: Name, msgType: Type) {}
    def delegation(function: Symbol, channels: List[Name]): SessionTypingEnvironment = {
      //todo: forbid delegation to
      this
    }
  }

  class TopLevelSessionTypingEnvironment extends SessionTypingEnvironment {
    val parent = null
  }

  class InProcessEnvironment
  (val parent: SessionTypingEnvironment, participant: Participant, localModel: ProtocolModel, sessChan: Name)
  extends SessionTypingEnvironment {
    override def leaveAccept: SessionTypingEnvironment = {
      println("leave accept: " + participant)
      parent
    }

    override def enterBranch(label: Type) : SessionTypingEnvironment = {
      println("enter branch: " + label)
      new InBranchEnvironment(this, participant, localModel, sessChan, label)
    }

    override def isSessionChannel(ident: Name): Boolean = {
      if (ident == sessChan) true
      else parent.isSessionChannel(ident)
    }
  }

  class InBranchEnvironment(parent: SessionTypingEnvironment, participant: Participant, localModel: ProtocolModel, sessChan: Name, branchLabel: Type)
  extends InProcessEnvironment(parent, participant, localModel, sessChan) {
    override def leaveBranch = {
      println("leave branch: " + branchLabel)
      // Todo: check session type is completed
      parent
    }

    override def leaveAccept: SessionTypingEnvironment = {
      error("Branch not finished, but leaving accept block")
      parent
    }
  }
}