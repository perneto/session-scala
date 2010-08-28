package uk.ac.ic.doc.sessionscala.compiler

import tools.nsc.Global
import org.scribble.common.logging.Journal
import org.scribble.protocol.projection.impl.ProtocolProjectorImpl
import org.scribble.protocol.model._
import org.scribble.protocol.conformance.ProtocolConformer
import org.scribble.protocol.conformance.impl.{BehaviourList, ProtocolConformerImpl}
import org.scribble.protocol.conformance.comparator.DefaultComparatorContext
import java.lang.String

trait SessionTypingEnvironments {
  val scribbleJournal: Journal
  val global: Global
  import global._

  val projector = new ProtocolProjectorImpl()

  class SessionEnvironmentException(msg: String) extends Exception(msg)

  trait SessionTypingEnvironment {
    val sharedChannels: Map[Name, ProtocolModel]
    val parent: SessionTypingEnvironment

    val protoModel = new ProtocolModel
    val protocol = new Protocol
    protoModel.setProtocol(protocol)

    protected def createInstance(sharedChannels: Map[Name, ProtocolModel]): SessionTypingEnvironment

    def isSessionChannel(c: Name) = false

    def registerSharedChannel(name: Name, globalType: ProtocolModel): SessionTypingEnvironment = {
      createInstance(sharedChannels + (name -> globalType))
    }

    def enterJoin(sharedChannel: Name, roleName: String, sessChan: Name): SessionTypingEnvironment = {
      println("enterJoin:" + sharedChannels)
      val role = new Role(roleName)
      val globalModel = getGlobalTypeForChannel(sharedChannel)
      val projectedModel = projector.project(globalModel, role, scribbleJournal)
      new InProcessEnvironment(sharedChannels, this, role, projectedModel, sessChan)
    }

    def leaveJoin : SessionTypingEnvironment = {
      throw new SessionEnvironmentException("trying to leave a join block, but was at top-level environment")
    }

    def enterBranch(label: Type) : SessionTypingEnvironment = {
      throw new SessionEnvironmentException("trying to enter a branch, but not in join block yet")
    }

    def leaveBranch : SessionTypingEnvironment = {
      throw new SessionEnvironmentException("trying to leave a branch, but was not in branch yet")
    }

    protected def getGlobalTypeForChannel(name: Name): ProtocolModel = {
      sharedChannels.get(name).getOrElse(
        if (parent == null)
            throw new SessionEnvironmentException("Channel: " + name + " is not in scope")
        else
          parent.getGlobalTypeForChannel(name)
      )
    }

    def send(sessChan: Name, role: String, msgType: Type): SessionTypingEnvironment = {
      throw new SessionEnvironmentException("trying to do a send operation, but not in join block yet")
    }

    def receive(sessChan: Name, role: String, msgType: Type): SessionTypingEnvironment = {
      throw new SessionEnvironmentException("trying to do a receive operation, but not in join block yet")
    }

    def delegation(function: Symbol, channels: List[Name]): SessionTypingEnvironment = {
      // todo: new env that forbids any use of s (delegated)
      this
    }
  }

  class TopLevelSessionTypingEnvironment(val sharedChannels: Map[Name, ProtocolModel]) extends SessionTypingEnvironment {
    val parent: SessionTypingEnvironment = null
    def this() = this(Map())

    def createInstance(sharedChans: Map[Name, ProtocolModel]): SessionTypingEnvironment = {
      new TopLevelSessionTypingEnvironment(sharedChans)
    }
  }

  class InProcessEnvironment
  (sharedChans: Map[Name, ProtocolModel], override val parent: SessionTypingEnvironment, role: Role, localModel: ProtocolModel, sessChan: Name)
  extends TopLevelSessionTypingEnvironment(sharedChans) {

    private def conforms(model: ProtocolModel, ref: ProtocolModel, journal: Journal): Boolean = {
      val mainBehaviourList = BehaviourList.createBehaviourList(model.getProtocol.getBlock)
		  val refBehaviourList = BehaviourList.createBehaviourList(ref.getProtocol.getBlock)

		  val context = new DefaultComparatorContext(null, null)
		  context.compare(mainBehaviourList, refBehaviourList, journal, true)
    }

    override def leaveJoin: SessionTypingEnvironment = {
      println("leave join: " + role)

      if (!conforms(protoModel, localModel, scribbleJournal))
        throw new SessionEnvironmentException("session not implemented properly")

      parent
    }

    override def enterBranch(label: Type) : SessionTypingEnvironment = {
      println("enter branch: " + label)
      new InBranchEnvironment(sharedChannels, this, role, localModel, sessChan, label)
    }

    override def isSessionChannel(ident: Name): Boolean = {
      if (ident == sessChan) true
      else parent.isSessionChannel(ident)
    }

    override def createInstance(newSharedChans: Map[Name, ProtocolModel]): SessionTypingEnvironment = {
      new InProcessEnvironment(newSharedChans, parent, role, localModel, sessChan)
    }

    def createInteraction(src: Role, dst: Role, msgType: String): Interaction = {
      val msg = new Interaction
      msg.setFromRole(src)
      msg.getToRoles.add(dst)
      val sig = new MessageSignature
      sig.getTypeReferences.add(new TypeReference(msgType))
      msg.setMessageSignature(sig)
      msg
    }

    def scalaToScribble(t: Type): String = {
      val s = t.toString // hack
      val end = {
        val i = s.indexOf('(')
        if (i > 0) i else s.length
      }
      val substring = s.substring(s.lastIndexOf('.')+1, end)
      println(substring)
      substring
    }

    override def send(sessChan: Name, dstRole: String, msgType: Type): SessionTypingEnvironment = {
      println("send: from " + role + " to " + dstRole + ": " + msgType)
      val msg = createInteraction(role, new Role(dstRole), scalaToScribble(msgType))
      println("send: " + msg)
      protocol.getBlock.add(msg)
      this
    }

    override def receive(sessChan: Name, srcRole: String, msgType: Type): SessionTypingEnvironment = {
      protocol.getBlock.add(createInteraction(new Role(srcRole), role, scalaToScribble(msgType)))
      this
    }
  }

  class InBranchEnvironment(sharedChans: Map[Name, ProtocolModel], parent: SessionTypingEnvironment, role: Role, localModel: ProtocolModel, sessChan: Name, branchLabel: Type)
  extends InProcessEnvironment(sharedChans, parent, role, localModel, sessChan) {
    override def leaveBranch = {
      println("leave branch: " + branchLabel)
      // Todo: check session type is completed
      parent
    }

    override def leaveJoin: SessionTypingEnvironment = {
      error("Branch not finished, but leaving join block")
      parent
    }

    override def createInstance(newSharedChans: Map[Name, ProtocolModel]): SessionTypingEnvironment = {
      new InBranchEnvironment(newSharedChans, parent, role, localModel, sessChan, branchLabel)
    }
  }
}