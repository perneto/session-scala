package uk.ac.ic.doc.sessionscala.compiler

import org.scribble.common.logging.Journal
import org.scribble.protocol.projection.impl.ProtocolProjectorImpl
import org.scribble.protocol.model._
import scalaj.collection.Imports._


/**
 * Created by: omp08
 */

trait CheckingEnvironments extends TypeCheckingUtils {
  self: SessionTypedElementsComponent with ScribbleModelFactories
          with CommonEnvironments with ScalaTypeSystemComponent =>

val scribbleJournal: Journal
  import global.{Block => _, _}

  class JoinBlocksPassTopLevelEnv(val ste: SessionTypedElements, val infEnv: InferredTypeRegistry) extends AbstractTopLevelEnv {
    def this() = this(EmptySTE, null)
    def this(ste: SessionTypedElements) = this(ste, null)
    def this(infEnv: InferredTypeRegistry) = this(EmptySTE, infEnv)

    def registerSharedChannel(name: Name, globalType: ProtocolModel, delegator: SessionTypingEnvironment): SessionTypingEnvironment =
      delegator.updated(delegator.ste.updated(name, globalType))

    override def enterJoin(delegator: SessionTypingEnvironment, sharedChannel: Name, roleName: String, sessChan: Name): SessionTypingEnvironment = {
      //println("enterJoin: " + ste)
      val role = new Role(roleName)
      val globalModel = delegator.getGlobalTypeForChannel(sharedChannel)
      val projector = new ProtocolProjectorImpl
      val projectedModel = projector.project(globalModel, role, scribbleJournal)
      new InProcessEnv(
        delegator.ste.updated(sessChan, new Session(typeSystem, projectedModel)),
        delegator, role, sessChan, infEnv)
    }

    def updated(ste: SessionTypedElements) = {
      assert(ste.sessions.values.map(_.isComplete).foldRight(true)(_&&_),
        "Top-level env should only be updated with completed sessions. ste: " + ste)
      new JoinBlocksPassTopLevelEnv(ste, infEnv)
    }

    override def send(sessChan: Name, role: String, msgSig: MsgSig, delegator: SessionTypingEnvironment) = notYet("send")
    override def receive(sessChan: Name, role: String, msgSig: MsgSig, delegator: SessionTypingEnvironment) = notYet("receive")

    override def enterChoiceReceiveBlock(delegator: SessionTypingEnvironment, sessChan: Name, srcRole: String) = notYet("choice receive")
    override def leaveChoiceReceiveBlock = notLeavingYet("choice receive")

    override def enterChoiceReceiveBranch(msgSig: MsgSig) = notYet("choice receive branch")
    override def leaveChoiceReceiveBranch = notLeavingYet("choice receive branch")
  }

  class FrozenChannelsEnv(val ste: SessionTypedElements, parent: SessionTypingEnvironment, frozenChannels: List[Name]) extends AbstractDelegatingEnv(parent) {
    def updated(newSte: SessionTypedElements) = new FrozenChannelsEnv(newSte, parent, frozenChannels)

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
        throw new SessionTypeCheckingException("Channel " + chan
                + " cannot be used anymore in this scope after it has been passed as a method parameter")
    }

    def checkFrozen(channels: List[Name]) {
      channels foreach (c => checkFrozen(c))
    }
  }

  class RecoverableTypeCheckingException(msg: String, val recoveryEnv: SessionTypingEnvironment) extends Exception(msg)

  class InProcessEnv(val ste: SessionTypedElements,
                     parent: SessionTypingEnvironment,
                     joinAsRole: Role,
                     sessChanJoin: Name,
                     infEnv: InferredTypeRegistry)
  extends AbstractDelegatingEnv(parent) {
    //println("Created InProcessEnv: " + ste)

    override def leaveJoin: SessionTypingEnvironment = {
      val session = ste.sessions(sessChanJoin)
      val newSte = ste.removeSession(sessChanJoin)
      val newEnv = parent.updated(newSte)
      println("leave join: " + joinAsRole + ", ste: " + ste + ", newSte: " + newSte)

      if (!session.isComplete)
        throw new RecoverableTypeCheckingException("Session not completed on channel "
              + sessChanJoin + " for role " + joinAsRole + ", remaining activities: " + session.remaining,
          newEnv)
      newEnv
    }

    override def isSessionChannel(ident: Name) = {
      ste.hasSessionChannel(ident) || parent.isSessionChannel(ident)
    }

    def updated(ste: SessionTypedElements) =
      new InProcessEnv(ste, parent, joinAsRole, sessChanJoin, infEnv)

    override def send(sessChan: Name, dstRoleName: String, msgSig: MsgSig, delegator: SessionTypingEnvironment): SessionTypingEnvironment = {
      val sess = delegator.ste.sessions(sessChan)
      val dstRole = new Role(dstRoleName)

      val newSess = sess.interaction(
        joinAsRole, dstRole, msgSig.toScribble)
      /*println(
        "send: on " + sessChan + " from " + joinAsRole + " to " +
        dstRole + ": " + msgType + ". Updated session: " + newSess
                + ", parent.ste.sessions: " + parent.ste.sessions + ", class: " + getClass())*/
      delegator.updated(sessChan, newSess)
    }

    override def receive(sessChan: Name, srcRole: String, msgSig: MsgSig, delegator: SessionTypingEnvironment): SessionTypingEnvironment = {
      val sess = delegator.ste.sessions(sessChan)

      val newSess = sess.interaction(
        new Role(srcRole), joinAsRole, msgSig.toScribble)
      /*println(
        "receive: on " + sessChan + " from " + srcRole + " to " +
        joinAsRole + ": " + msgType + ". Updated session: " + newSess)*/
      delegator.updated(sessChan, newSess)
    }

    override def enterChoiceReceiveBlock(delegator: SessionTypingEnvironment, sessChan: Name, srcRole: String) =
      new ChoiceReceiveBlockEnv(delegator.ste, delegator, sessChan, srcRole, Nil, None)

    override def enterThen(delegator: SessionTypingEnvironment) = new ThenBlockEnv(delegator.ste, delegator)

    def retrieveInferred(method: Symbol, delegatedChans: List[Name], returnedChans: List[Name]):
    Seq[(Name, Int, LabelledBlock, Option[Name])] =
    {
      val retChansOptions = returnedChans map (Some(_))
      // delegatedChans is always the same length or longer than returnedChans,
      // so the null parameter to zipAll is never used
      val zippedList = delegatedChans.zipAll(retChansOptions, null, None)
      (zippedList.view.zipWithIndex) map { case ((chan, retChanOption), index) =>
        (chan, index, infEnv.inferredSessionType(method, index), retChanOption)
      }
    }

    override def delegation(delegator: SessionTypingEnvironment, method: Symbol, delegatedChans: List[Name], returnedChans: List[Name]): SessionTypingEnvironment = {
      val inferred = retrieveInferred(method, delegatedChans, returnedChans)
      println(inferred)
      val updated = (inferred foldLeft delegator) {
        case (env, (chan, chanRank, recur, retChanOpt)) =>
          val sess = env.ste.sessions(chan)
          val advancedSession = advanceOne(chanRank, sess, recur, Nil)
          retChanOpt match {
            case Some(retChan) =>
              env.updated(retChan, advancedSession).updated(chan, new Session(sess, List[Activity]() asJava))
            case None =>
              env.updated(chan, advancedSession)
          }
      }
      new FrozenChannelsEnv(updated.ste, updated, delegatedChans)
    }

    def advanceList(chanRank: Int, sess: Session, acts: Seq[Activity], replacedLabels: List[String]): Session =
      (acts foldLeft sess) (advanceOne(chanRank, _, _, replacedLabels))

    def advanceOne(chanRank: Int, sess: Session, act: Activity, replacedLabels: List[String]): Session = act match {
      case i: Interaction => sendOrReceive(sess, i)
      case c: Choice =>
        /*
        if (isChoiceReceive(c)) {
          // todo: it's legal to have more branches than specified in a choice receive
        } else {
	      } */
        val src = c.getFromRole
        val dst = c.getToRole
        // we iterate on c's branches, so this supports sending less branches than specified for a choice send
        c.getWhens foreach { infWhen =>
          val sessBranch = sess.findMatchingWhen(src, dst, infWhen)
          advanceList(chanRank, sessBranch, infWhen.getBlock.getContents.asScala, replacedLabels)
        }
        sess.dropFirst
      case r: LabelledBlock =>
        val expectedRecur = sess.getRecur
        if (expectedRecur != null) { // genuine expected recursion in spec
          // don't unroll, just jump into recur contents (otherwise infinite loop)
          val renamedSpec = alphaRename(contents(expectedRecur).asScala, expectedRecur.getLabel, r.getLabel)
          advanceList(chanRank, new Session(sess, renamedSpec.asJava),
            contents(r).asScala, r.getLabel :: replacedLabels)
          sess.dropFirst
        } else { // this is an unnecessary inferred recursion, following the general inference scheme
          advanceList(chanRank, sess, unroll(r), replacedLabels)
        }
      case r: Recursion =>
        println("Recursion: " + r.getLabel + ", replacedLabels: " + replacedLabels)
        if (replacedLabels contains r.getLabel) sess.dropMatchingRecursionLabel(r)
        else {
          // fixme: this doesn't work when there are variable numbers of channels as arguments, or
          // when formal and effective parameter names are different. Need to generate a unique label for each
          // channel the method takes as a parameter. With 1 param everywhere, this works, but only because
          // chanRank is 0 all the time.
          val method = infEnv.methodFor(r.getLabel)
          assert(notEmpty(infEnv.inferredSessionType(method, chanRank)), "Calling method: "
                + method + ": No inferred session type for channel rank: " + chanRank + " in env: " + infEnv)
          val recur = infEnv.inferredSessionType(method, chanRank)
          println("inferred for " + method + ", chanRank: " + chanRank + ": " + recur)
          advanceOne(chanRank, sess, recur, replacedLabels)
        }
    }

    def contents(r: LabelledBlock) = r.getBlock.getContents

    // Similar to normal typechecking, this can be a choice selection as well as a normal interaction
    def sendOrReceive(sess: Session, i: Interaction) = {
      val msig = i.getMessageSignature
      val src = i.getFromRole
      val dsts = i.getToRoles
      val dst = if (dsts.isEmpty) null else dsts.get(0)
      println("sendOrReceive - " + i + ", msig: " + msig)
      sess.interaction(src, dst, msig)
    }

    override def branchComplete(parentSte: SessionTypedElements, chan: Name, branch1: SessionTypedElements, branch2: SessionTypedElements, label: MsgSig) = {
      checkSessionsRemainingSame(branch1.sessions, branch2.sessions)
      branch1
    }
  }

  class ChoiceReceiveBlockEnv(val ste: SessionTypedElements,
                              parent: SessionTypingEnvironment,
                              chanChoice: Name,
                              choiceSrc: String,
                              branches: List[MsgSig],
                              lastBranchSte: Option[SessionTypedElements])
  extends AbstractDelegatingEnv(parent) {

    //println("Created ChoiceReceiveBlockEnv: " + ste.sessions)

    def parentSession = ste.sessions(chanChoice)

    override def enterChoiceReceiveBranch(msgSig: MsgSig) = {
      // The choice activity at the beginning
      // of parentSession will only be removed when all branches
      // have been visited and correctly typechecked.
      val labelSignature = msgSig.toScribble
      val sessBranch = parentSession.visitBranch(
        labelSignature,
        new Role(choiceSrc))
      val newSte = ste.updated(chanChoice, sessBranch)

      val updatedThis = new ChoiceReceiveBlockEnv(ste, parent, chanChoice,
        choiceSrc, msgSig :: branches, lastBranchSte)
      new ChoiceReceiveBranchEnv(newSte, updatedThis, chanChoice,
          branches, msgSig, lastBranchSte)
    }

    def updated(ste: SessionTypedElements) =
      new ChoiceReceiveBlockEnv(ste, parent, chanChoice,
          choiceSrc, branches, lastBranchSte)

    def withLastBranchSte(lastBranchSte: SessionTypedElements): ChoiceReceiveBlockEnv = {
      new ChoiceReceiveBlockEnv(ste, parent, chanChoice,
          choiceSrc, branches, Some(lastBranchSte))
    }

    override def leaveChoiceReceiveBlock = {
      //println("seen branches: " + branches)
      val missing = parentSession.missingBranches(
        branches map (l => l.toScribble)
        asJava
      )
      if (!missing.isEmpty)
        throw new SessionTypeCheckingException("Missing choice receive branch(es): " + missing)

      // to keep advance of interleaved sessions on other channels than chanChoice
      val newSte = lastBranchSte.get
      // lastBranchSte.sessions.get(chanChoice) is empty as it only had the branch block
      // now we replace it by the parent session which still had the whole thing,
      // only removing the choice construct at the beginning
      parent.updated(
        newSte.updated(chanChoice, parentSession.dropFirst))
    }
  }

  class ChoiceReceiveBranchEnv(val ste: SessionTypedElements,
                               parent: ChoiceReceiveBlockEnv,
                               chanChoice: Name,
                               branches: List[MsgSig],
                               branchLabel: MsgSig,
                               lastBranchSte: Option[SessionTypedElements])
          extends AbstractDelegatingEnv(parent) {

    //println("Created ChoiceReceiveBranchEnv: " + ste.sessions)

    override def leaveChoiceReceiveBranch = {
      //println("leave branch: " + branchLabel)
      val sess = ste.sessions(chanChoice)
      if (!sess.isComplete) // sess only has the branch block, not what comes after it
        throw new SessionTypeCheckingException("Branch incomplete, missing: "+ sess.remaining)

      val nextSte = if (lastBranchSte.isDefined) {
        branchComplete(parent.ste, chanChoice, ste, lastBranchSte.get, branchLabel)
        //println("checked last branch ok: " + lastBranchSess.get)
      } else ste

      parent.withLastBranchSte(nextSte)
    }

    def updated(ste: SessionTypedElements) =
      new ChoiceReceiveBranchEnv(ste, parent, chanChoice,
          branches, branchLabel, lastBranchSte)
  }

  class ThenBlockEnv(val ste: SessionTypedElements,
                     parent: SessionTypingEnvironment)
      extends AbstractDelegatingEnv(parent) {

    //println("Created ThenBlockEnv: " + ste.sessions + ", parent.ste.sessions: " + parent.ste.sessions)

    override def updated(newSte: SessionTypedElements) =
      new ThenBlockEnv(newSte, parent)

    override def enterElse = {
      //println("enterElse, parent: " + parent + ", parent.ste.sessions: " + parent.ste.sessions)
      new ElseBlockEnv(parent.ste, parent, ste)
    }
  }

}