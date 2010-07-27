package uk.ac.ic.doc.sessionscala.compiler

import tools.nsc.Global
import org.scribble.common.logging.Journal
import org.scribble.protocol.projection.impl.ProtocolProjectorImpl
import org.scribble.protocol.model.{Role, Participant, ProtocolModel}

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

    def enterAccept(sharedChannel: Name, roleName: String, sessChan: Name): SessionTypingEnvironment = {
      println("enterAccept:" + sharedChannels)
      val role = new Role(roleName)
      val globalModel = getGlobalTypeForChannel(sharedChannel)
      val projectedModel = projector.project(globalModel, role, scribbleJournal)
      println(projectedModel)
      return new InProcessEnvironment(this, role, projectedModel, sessChan)
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

    def send(sessChan: Name, role: String, msgType: Type) = this
    def receive(sessChan: Name, msgType: Type) = this
    def delegation(function: Symbol, channels: List[Name]): SessionTypingEnvironment = {
      // todo: new env that forbids any use of s (delegated)
      this
    }
  }

  class TopLevelSessionTypingEnvironment extends SessionTypingEnvironment {
    val parent = null
  }

  class InProcessEnvironment
  (val parent: SessionTypingEnvironment, role: Role, localModel: ProtocolModel, sessChan: Name)
  extends SessionTypingEnvironment {
    override def leaveAccept: SessionTypingEnvironment = {
      println("leave accept: " + role)
      parent
    }

    override def enterBranch(label: Type) : SessionTypingEnvironment = {
      println("enter branch: " + label)
      new InBranchEnvironment(this, role, localModel, sessChan, label)
    }

    override def isSessionChannel(ident: Name): Boolean = {
      if (ident == sessChan) true
      else parent.isSessionChannel(ident)
    }
  }

  class InBranchEnvironment(parent: SessionTypingEnvironment, role: Role, localModel: ProtocolModel, sessChan: Name, branchLabel: Type)
  extends InProcessEnvironment(parent, role, localModel, sessChan) {
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