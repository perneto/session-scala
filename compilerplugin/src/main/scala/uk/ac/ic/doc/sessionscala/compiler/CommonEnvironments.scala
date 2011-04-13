package uk.ac.ic.doc.sessionscala.compiler

import tools.nsc.Global
import uk.ac.ic.doc.sessionscala.ScribbleRuntimeUtils
import org.scribble.protocol.model.{Role, RecBlock, ProtocolModel}

/**
 * Created by: omp08
 */

trait CommonEnvironments {
  self: SessionTypedElementsComponent 
          with ScalaTypeSystemComponent
          with ScribbleCompilerUtils =>

  val global: Global
  import global._

  trait InferredTypeRegistry {
    def inferredSessionType(method: Symbol, rank: Int): RecBlock
    def returnRank(method: Symbol, rank: Int): Option[Int]
    def inferredSessionType(label: String): RecBlock
  }

  val branchesUneven = "All branches of a branching statement should advance the session evenly."

// todo: split out checking specific and inference specific parts. separate inferred() into other trait
// todo: crash by default when not in right environment
  trait SessionTypingEnvironment {
    val ste: SessionTypedElements
    val parent: SessionTypingEnvironment

    def updated(ste: SessionTypedElements): SessionTypingEnvironment
    def updated(sessChan: Name, newSess: Session): SessionTypingEnvironment = {
      //println("update: " + sessChan + " -> " + newSess + ", class: " + getClass)
      updated(ste.updated(sessChan, newSess))
    }

    def isSessionChannel(c: Name): Boolean
    def isSharedChannel(c: Name): Boolean

    def registerAddress(name: Name, globalType: ProtocolModel, roleName: String): SessionTypingEnvironment = {
      //println("registerAddress: " + this)
      registerAddress(name, globalType, roleName, this)
    }
    def registerAddress(name: Name, globalType: ProtocolModel, roleName: String, delegator: SessionTypingEnvironment): SessionTypingEnvironment

    def invite(sharedChans: List[Name]): SessionTypingEnvironment = invite(sharedChans, this)
    def invite(sharedChans: List[Name], delegator: SessionTypingEnvironment): SessionTypingEnvironment = delegator

    def enterJoin(sharedChannel: Name, sessChan: Name): SessionTypingEnvironment =
      enterJoin(this, sharedChannel, sessChan)
    def enterJoin(delegator: SessionTypingEnvironment, sharedChannel: Name, sessChan: Name): SessionTypingEnvironment = delegator
    def leaveJoin: SessionTypingEnvironment = this

    def getGlobalTypeForChannel(name: Name): (ProtocolModel, Role) =
      ste.addressOfName(name).getOrElse(
        if (parent == null)
          throw new SessionTypeCheckingException("Channel: " + name + " is not in scope")
        else
          parent.getGlobalTypeForChannel(name))

    def send(sessChan: Name, role: String, msgSig: MsgSig): SessionTypingEnvironment =
      send(sessChan, role, msgSig, this)
    def send(sessChan: Name, role: String, msgSig: MsgSig, delegator: SessionTypingEnvironment): SessionTypingEnvironment = delegator
    def receive(sessChan: Name, role: String, msgSig: MsgSig): SessionTypingEnvironment =
      receive(sessChan, role, msgSig, this)
    def receive(sessChan: Name, role: String, msgSig: MsgSig, delegator: SessionTypingEnvironment): SessionTypingEnvironment = delegator

    def enterChoiceReceiveBlock(delegator: SessionTypingEnvironment, sessChan: Name, srcRole: Option[String]): SessionTypingEnvironment = delegator
    def enterChoiceReceiveBlock(sessChan: Name, srcRole: Option[String]): SessionTypingEnvironment =
      enterChoiceReceiveBlock(this, sessChan, srcRole)

    def enterChoiceReceiveBranch(srcRole: Option[String], msgSig: MsgSig): SessionTypingEnvironment = this
    def leaveChoiceReceiveBranch: SessionTypingEnvironment = this
    def leaveChoiceReceiveBlock: SessionTypingEnvironment = this

    def enterThen: SessionTypingEnvironment = enterThen(this)
    def enterThen(delegator: SessionTypingEnvironment): SessionTypingEnvironment = {
      //println("enterThen:" + this + ", delegator: " + delegator)
      delegator
    }
    def enterElse: SessionTypingEnvironment = enterElse(this)
    def enterElse(delegator: SessionTypingEnvironment): SessionTypingEnvironment = {
      //println("enterElse:" + this+ ", delegator: " + delegator)
      delegator
    }
    def leaveIf: SessionTypingEnvironment = leaveIf(this)
    def leaveIf(delegator: SessionTypingEnvironment): SessionTypingEnvironment = {
      //println("leaveIf:" + this + ", delegator: " + delegator)
      delegator
    }

    def enterLoop: SessionTypingEnvironment = new FrozenChannelsEnv(ste, this, ste.sessions.keysIterator, ste.addresses.keysIterator, "loop")
    def leaveLoop: SessionTypingEnvironment = parent.updated(ste)

    def enterClosure(params: List[Name]): SessionTypingEnvironment = {
      //println("enter closure: " + this + ", params: " + params + ", frozen: " + (ste.sessions.keySet -- params))
      new FrozenChannelsEnv(
      ste, this, (ste.sessions.keySet -- params).iterator, (ste.addresses.keySet -- params).iterator, "closure")
    }
    def leaveClosure: SessionTypingEnvironment = {
      //println("leave closure: " + this + ", parent: " + parent + ", ste: " + ste)
      parent.updated(ste)
    }

    def returnStatement: SessionTypingEnvironment = returnStatement(this)
    def returnStatement(delegator: SessionTypingEnvironment): SessionTypingEnvironment = delegator

    def delegation(function: Symbol, channels: List[Name], returnedChannels: List[Name]): SessionTypingEnvironment =
      delegation(this, function, channels, returnedChannels)
    def delegation(delegator: SessionTypingEnvironment, function: Symbol, channels: List[Name], returnedChannels: List[Name]): SessionTypingEnvironment = delegator

    def enterSessionMethod(fun: Symbol, sessChans: List[Name]): SessionTypingEnvironment = enterSessionMethod(this, fun, sessChans)
    def enterSessionMethod(delegator: SessionTypingEnvironment, fun: Symbol, sessChans: List[Name]): SessionTypingEnvironment = delegator
    def leaveSessionMethod(returnedChans: List[Name]): SessionTypingEnvironment = throw new IllegalStateException("not in session method")

    def branchComplete(parentSte: SessionTypedElements, chan: Name, branch1: SessionTypedElements, branch2: SessionTypedElements, label: MsgSig): SessionTypedElements = ste

    def inferred: InferredTypeRegistry = throw new IllegalStateException("this environment does not have inferred types")
  }

  abstract class AbstractDelegatingEnv(val parent: SessionTypingEnvironment)
  extends SessionTypingEnvironment {
    def isSessionChannel(c: Name) = parent.isSessionChannel(c)
    def isSharedChannel(c: Name) = parent.isSharedChannel(c)

    def registerAddress(name: Name, globalType: ProtocolModel, roleName: String, delegator: SessionTypingEnvironment): SessionTypingEnvironment =
      parent.registerAddress(name, globalType, roleName, delegator)

    override def invite(sharedChans: List[Name], delegator: SessionTypingEnvironment) =
      parent.invite(sharedChans, delegator)

    override def enterJoin(delegator: SessionTypingEnvironment, sharedChannel: Name, sessChan: Name) =
      parent.enterJoin(delegator, sharedChannel, sessChan)
    override def leaveJoin = parent.leaveJoin

    override def send(sessChan: Name, role: String, msgSig: MsgSig, delegator: SessionTypingEnvironment): SessionTypingEnvironment =
      parent.send(sessChan, role, msgSig, delegator)

    override def receive(sessChan: Name, role: String, msgSig: MsgSig, delegator: SessionTypingEnvironment) =
      parent.receive(sessChan, role, msgSig, delegator)

    override def enterChoiceReceiveBlock(delegator: SessionTypingEnvironment, sessChan: Name, srcRole: Option[String]) =
      parent.enterChoiceReceiveBlock(delegator, sessChan, srcRole)
    override def enterChoiceReceiveBranch(srcRole: Option[String], msgSig: MsgSig) =
      parent.enterChoiceReceiveBranch(srcRole, msgSig)
    override def leaveChoiceReceiveBranch = parent.leaveChoiceReceiveBranch
    override def leaveChoiceReceiveBlock = parent.leaveChoiceReceiveBlock

    override def enterThen(delegator: SessionTypingEnvironment) = parent.enterThen(delegator)
    override def enterElse(delegator: SessionTypingEnvironment) = parent.enterElse(delegator)
    override def leaveIf(delegator: SessionTypingEnvironment) = parent.leaveIf(delegator)

    override def delegation(delegator: SessionTypingEnvironment, function: Symbol, channels: List[Name], returnedChannels: List[Name]) =
      parent.delegation(delegator, function, channels, returnedChannels)

    override def enterSessionMethod(delegator: SessionTypingEnvironment, fun: Symbol, sessChans: List[Name]) = parent.enterSessionMethod(delegator, fun, sessChans)

    override def branchComplete(parentSte: SessionTypedElements, chan: Name, branch1: SessionTypedElements, branch2: SessionTypedElements, label: MsgSig) =
      parent.branchComplete(parentSte, chan, branch1, branch2, label)

    override def returnStatement(delegator: SessionTypingEnvironment) = parent.returnStatement(delegator)
  }

  abstract class AbstractTopLevelEnv(val parent: SessionTypingEnvironment) extends SessionTypingEnvironment {
    override def isSessionChannel(c: Name) = false
    override def isSharedChannel(c: Name) = false

    override def registerAddress(name: Name, globalType: ProtocolModel, roleName: String, delegator: SessionTypingEnvironment): SessionTypingEnvironment = {
      val role = new Role(roleName)
      val roles = ScribbleRuntimeUtils.roles(globalType)
      if (!roles.contains(role))
        throw new SessionTypeCheckingException("Protocol does not contain role "+roleName)
      // TODO: Validate protocol here, including projectability checks
      delegator.updated(delegator.ste.withAddress(name, globalType, role))
    }

    protected def notYet(what: String) =
      throw new SessionTypeCheckingException("trying to do a " + what
        + " operation, but not in appropriate block yet")
    protected def notLeavingYet(what: String) =
      throw new SessionTypeCheckingException("trying to leave a " + what
        + " block, but was not yet in such a block environment")
  }

  class ElseBlockEnv(val ste: SessionTypedElements,
                     parent: SessionTypingEnvironment,
                     steThenBranch: SessionTypedElements)
      extends AbstractDelegatingEnv(parent) {

    //println("Created ElseBlockEnv: " + ste + " sessionsThenBranch: "
    //        + sessionsThenBranch + ", parent.ste.sessions: " + parent.ste.sessions)

    override def leaveIf = {
      val mergedSte = branchComplete(parent.ste, null, steThenBranch, ste, null)
      //println(mergedSte)
      //println(parent)
      parent.updated(mergedSte)
    }

    def updated(newSte: SessionTypedElements) =
      new ElseBlockEnv(newSte, parent, steThenBranch)
  }

  /** TODO: Investigate use/non-use of delegator variants here, looks buggy */
  class FrozenChannelsEnv(val ste: SessionTypedElements, parent: SessionTypingEnvironment,
                          frozenSessionChannels: Iterator[Name],
                          frozenSharedChannels: Iterator[Name], location: String) extends AbstractDelegatingEnv(parent)
  {
    def updated(newSte: SessionTypedElements) = new FrozenChannelsEnv(
      newSte, parent, frozenSessionChannels, frozenSharedChannels, location)

    override def invite(sharedChans: List[Name]) = {
      sharedChans foreach checkSharedFrozen
      parent.invite(sharedChans, this)
    }

    override def send(sessChan: Name, role: String, msgSig: MsgSig) = {
      checkFrozen(sessChan)
      parent.send(sessChan, role, msgSig)
    }

    override def receive(sessChan: Name, role: String, msgSig: MsgSig) = {
      checkFrozen(sessChan)
      parent.receive(sessChan, role, msgSig)
    }

    override def delegation(function: Symbol, channels: List[Name], returnedChannels: List[Name]) = {
      checkFrozen(channels)
      parent.delegation(function, channels, returnedChannels)
    }

    override def enterChoiceReceiveBlock(sessChan: Name, srcRole: Option[String]) = {
      checkFrozen(sessChan)
      parent.enterChoiceReceiveBlock(sessChan, srcRole)
    }

    def checkSharedFrozen(chan: Name) {
      checkFrozen(chan, frozenSharedChannels, " cannot be used for invites in a " + location)
    }

    def checkFrozen(chan: Name) {
      checkFrozen(chan, frozenSessionChannels, " cannot be used in a " + location)
    }

    def checkFrozen(chan: Name, frozen: Iterator[Name], reason: String) {
      if (frozen.contains(chan))
        throw new SessionTypeCheckingException("Channel " + chan + reason)
    }

    def checkFrozen(channels: List[Name]) {
      channels foreach (c => checkFrozen(c))
    }
  }

  def illegalReturn() = throw new SessionTypeCheckingException(
    "Return statements are not allowed inside session methods or bind/join blocks")
}