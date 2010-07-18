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

    def isSessionChannel(c: Any) = true // Todo

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
      return new InProcessEnvironment(this, participant, projectedModel)
    }

    def leaveAccept : SessionTypingEnvironment = {
      throw new IllegalStateException("this should not happen")
    }

    def enterBranch(label: Tree) : SessionTypingEnvironment = {
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
  }

  class TopLevelSessionTypingEnvironment extends SessionTypingEnvironment {
    val parent = null
  }

  class InProcessEnvironment(val parent: SessionTypingEnvironment, participant: Participant, localModel: ProtocolModel) extends SessionTypingEnvironment {
    override def leaveAccept = {
      println("leave accept: " + participant)
      // Todo: check session type is completed
      parent
    }

    override def enterBranch(label: Tree) : SessionTypingEnvironment = {
      println("enter branch: " + label)
      new InBranchEnvironment(this, label)
    }
  }

  class InBranchEnvironment(val parent: SessionTypingEnvironment, branchLabel: Tree) extends SessionTypingEnvironment {
    override def leaveBranch = {
      println("leave branch: " + branchLabel)
      // Todo: check session type is completed
      parent
    }
  }
}