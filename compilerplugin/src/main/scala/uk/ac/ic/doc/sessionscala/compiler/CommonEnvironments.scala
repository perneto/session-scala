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
    def methodFor(label: String): Symbol
  }

  val branchesUneven = "All branches of a branching statement should advance the session evenly."

// todo: split out checking specific and inference specific parts. separate getInferred into other trait
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
    def enterThen(delegator: SessionTypingEnvironment): SessionTypingEnvironment = this
    def enterElse: SessionTypingEnvironment = this
    def leaveIf: SessionTypingEnvironment = this

    def delegation(function: Symbol, channels: List[Name], returnedChannels: List[Name]): SessionTypingEnvironment =
      delegation(this, function, channels, returnedChannels)
    def delegation(delegator: SessionTypingEnvironment, function: Symbol, channels: List[Name], returnedChannels: List[Name]): SessionTypingEnvironment = this

    def enterSessionMethod(fun: Symbol, sessChans: List[Name]): SessionTypingEnvironment = this
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
    override def enterElse = parent.enterElse
    override def leaveIf = parent.leaveIf

    override def delegation(delegator: SessionTypingEnvironment, function: Symbol, channels: List[Name], returnedChannels: List[Name]) =
      parent.delegation(delegator, function, channels, returnedChannels)

    override def branchComplete(parentSte: SessionTypedElements, chan: Name, branch1: SessionTypedElements, branch2: SessionTypedElements, label: MsgSig) =
      parent.branchComplete(parentSte, chan, branch1, branch2, label)
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


}