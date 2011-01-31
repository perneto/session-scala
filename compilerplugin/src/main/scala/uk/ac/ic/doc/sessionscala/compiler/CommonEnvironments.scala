package uk.ac.ic.doc.sessionscala.compiler

import tools.nsc.Global
import org.scribble.protocol.model.{LabelledBlock, ProtocolModel}

/**
 * Created by: omp08
 */

trait CommonEnvironments {
  self: SessionTypedElementsComponent with ScalaTypeSystemComponent =>

  val global: Global
  import global._

  trait InferredTypeRegistry {
    def inferredSessionType(method: Symbol, rank: Int): LabelledBlock
    def returnRank(method: Symbol, rank: Int): Option[Int]
    def methodFor(label: String): Symbol
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

    def registerSharedChannel(name: Name, globalType: ProtocolModel): SessionTypingEnvironment =
      registerSharedChannel(name, globalType, this)
    def registerSharedChannel(name: Name, globalType: ProtocolModel, delegator: SessionTypingEnvironment): SessionTypingEnvironment

    def enterJoin(sharedChannel: Name, roleName: String, sessChan: Name): SessionTypingEnvironment =
      enterJoin(this, sharedChannel, roleName, sessChan)
    def enterJoin(delegator: SessionTypingEnvironment, sharedChannel: Name, roleName: String, sessChan: Name): SessionTypingEnvironment = this
    def leaveJoin: SessionTypingEnvironment = this

    def getGlobalTypeForChannel(name: Name): ProtocolModel =
      ste.getSharedChan(name).getOrElse(
        if (parent == null)
          throw new SessionTypeCheckingException("Channel: " + name + " is not in scope")
        else
          parent.getGlobalTypeForChannel(name))

    def send(sessChan: Name, role: String, msgSig: MsgSig): SessionTypingEnvironment =
      send(sessChan, role, msgSig, this)
    def send(sessChan: Name, role: String, msgSig: MsgSig, delegator: SessionTypingEnvironment): SessionTypingEnvironment = this
    def receive(sessChan: Name, role: String, msgSig: MsgSig): SessionTypingEnvironment =
      receive(sessChan, role, msgSig, this)
    def receive(sessChan: Name, role: String, msgSig: MsgSig, delegator: SessionTypingEnvironment): SessionTypingEnvironment = this

    def enterChoiceReceiveBlock(delegator: SessionTypingEnvironment, sessChan: Name, srcRole: String): SessionTypingEnvironment = this
    def enterChoiceReceiveBlock(sessChan: Name, srcRole: String): SessionTypingEnvironment =
      enterChoiceReceiveBlock(this, sessChan, srcRole)

    def enterChoiceReceiveBranch(msgSig: MsgSig): SessionTypingEnvironment = this
    def leaveChoiceReceiveBranch: SessionTypingEnvironment = this
    def leaveChoiceReceiveBlock: SessionTypingEnvironment = this

    def enterThen: SessionTypingEnvironment = enterThen(this)
    def enterThen(delegator: SessionTypingEnvironment): SessionTypingEnvironment = {println("enterThen:" + this + ", delegator: " + delegator); delegator}
    def enterElse: SessionTypingEnvironment = enterElse(this)
    def enterElse(delegator: SessionTypingEnvironment): SessionTypingEnvironment = {println("enterElse:" + this+ ", delegator: " + delegator); delegator}
    def leaveIf: SessionTypingEnvironment = leaveIf(this)
    def leaveIf(delegator: SessionTypingEnvironment): SessionTypingEnvironment = {println("leaveIf:" + this + ", delegator: " + delegator); delegator}

    def enterLoop: SessionTypingEnvironment = new FrozenChannelsEnv(ste, this, ste.sessions.keysIterator, "cannot be used in a loop")
    def leaveLoop: SessionTypingEnvironment = parent.updated(ste)

    def enterClosure(params: List[Name]): SessionTypingEnvironment = {
      println("enter closure: " + this + ", params: " + params + ", frozen: " + (ste.sessions.keySet -- params))
      new FrozenChannelsEnv(
      ste, this, (ste.sessions.keySet -- params).iterator, "cannot be used in a closure")
    }
    def leaveClosure: SessionTypingEnvironment = {
      println("leave closure: " + this + ", parent: " + parent + ", ste: " + ste)
      parent.updated(ste)
    }

    def returnStatement: SessionTypingEnvironment = returnStatement(this)
    def returnStatement(delegator: SessionTypingEnvironment): SessionTypingEnvironment = delegator

    def delegation(function: Symbol, channels: List[Name], returnedChannels: List[Name]): SessionTypingEnvironment =
      delegation(this, function, channels, returnedChannels)
    def delegation(delegator: SessionTypingEnvironment, function: Symbol, channels: List[Name], returnedChannels: List[Name]): SessionTypingEnvironment = this

    def enterSessionMethod(fun: Symbol, sessChans: List[Name]): SessionTypingEnvironment = enterSessionMethod(this, fun, sessChans)
    def enterSessionMethod(delegator: SessionTypingEnvironment, fun: Symbol, sessChans: List[Name]): SessionTypingEnvironment = this
    def leaveSessionMethod(returnedChans: List[Name]): SessionTypingEnvironment = this

    def branchComplete(parentSte: SessionTypedElements, chan: Name, branch1: SessionTypedElements, branch2: SessionTypedElements, label: MsgSig): SessionTypedElements = throw new IllegalStateException

    def inferred: InferredTypeRegistry = throw new IllegalStateException("this environment does not have inferred types")
  }

  abstract class AbstractDelegatingEnv(val parent: SessionTypingEnvironment)
  extends SessionTypingEnvironment {
    def isSessionChannel(c: Name) = parent.isSessionChannel(c)

    def registerSharedChannel(name: Name, globalType: ProtocolModel, delegator: SessionTypingEnvironment): SessionTypingEnvironment =
      parent.registerSharedChannel(name, globalType, delegator)

    override def enterJoin(delegator: SessionTypingEnvironment, sharedChannel: Name, roleName: String, sessChan: Name) =
      parent.enterJoin(delegator, sharedChannel, roleName, sessChan)
    override def leaveJoin = parent.leaveJoin

    override def send(sessChan: Name, role: String, msgSig: MsgSig, delegator: SessionTypingEnvironment): SessionTypingEnvironment =
      parent.send(sessChan, role, msgSig, delegator)

    override def receive(sessChan: Name, role: String, msgSig: MsgSig, delegator: SessionTypingEnvironment) =
      parent.receive(sessChan, role, msgSig, delegator)

    override def enterChoiceReceiveBlock(delegator: SessionTypingEnvironment, sessChan: Name, srcRole: String) =
      parent.enterChoiceReceiveBlock(delegator, sessChan, srcRole)
    override def enterChoiceReceiveBranch(msgSig: MsgSig) =
      parent.enterChoiceReceiveBranch(msgSig)
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

  abstract class AbstractTopLevelEnv extends SessionTypingEnvironment {
    val parent = null
    override def isSessionChannel(c: Name) = false

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

  class FrozenChannelsEnv(val ste: SessionTypedElements, parent: SessionTypingEnvironment,
                          frozenChannels: Iterator[Name], reason: String) extends AbstractDelegatingEnv(parent) {
      def updated(newSte: SessionTypedElements) = new FrozenChannelsEnv(newSte, parent, frozenChannels, reason)

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

      override def enterChoiceReceiveBlock(sessChan: Name, srcRole: String) = {
        checkFrozen(sessChan)
        parent.enterChoiceReceiveBlock(sessChan, srcRole)
      }

      def checkFrozen(chan: Name) {
        if (frozenChannels.contains(chan))
          throw new SessionTypeCheckingException("Channel " + chan + " " + reason)
      }

      def checkFrozen(channels: List[Name]) {
        channels foreach (c => checkFrozen(c))
      }
    }

  def illegalReturn() = throw new SessionTypeCheckingException(
    "Return statements are not allowed inside session methods or accept/join blocks")
}