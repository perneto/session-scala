package uk.ac.ic.doc.sessionscala.compiler

import org.scribble.common.logging.Journal
import org.scribble.protocol.model._
import scalaj.collection.Imports._
import uk.ac.ic.doc.sessionscala.ScribbleRuntimeUtils


/**
 * Created by: omp08
 */

trait CheckingEnvironments extends TypeCheckingUtils {
  self: SessionTypedElementsComponent with ScribbleCompilerUtils
          with CommonEnvironments with ScalaTypeSystemComponent =>

val scribbleJournal: Journal
  import global.{Block => _, _}

  class ProcessBlocksPassTopLevelEnv(val ste: SessionTypedElements, val infEnv: InferredTypeRegistry) extends AbstractTopLevelEnv(null) {
    def this() = this(EmptySTE, null)
    def this(ste: SessionTypedElements) = this(ste, null)
    def this(infEnv: InferredTypeRegistry) = this(EmptySTE, infEnv)

    override def isSharedChannel(c: Name) = ste.addresses.contains(c)

    override def invite(addresses: List[Name], delegator: SessionTypingEnvironment) = {
      //println("invite: "+this+", delegator: "+delegator+", delegator.ste: "+delegator.ste)
      if (addresses.length == 0)
        throw new SessionTypeCheckingException("startSession should have at least one argument")
      
      var fstProto: ProtocolModel = null
      var roles: Set[Role] = null
      addresses foreach { addr =>
        val (proto, role) = delegator.ste.addresses(addr)
        if (fstProto == null)
          fstProto = proto
        // TODO: check protocols are same, implement equals on ProtocolModel
        if (roles == null)
          roles = ScribbleRuntimeUtils.roles(proto)
        if (!roles.contains(role))
          throw new SessionTypeCheckingException("Role "+role+
                  " was already invited for this session") // role in protocol already checked in registerAddress
        else roles -= role
      }
      delegator
    }

    override def enterJoin(delegator: SessionTypingEnvironment, sharedChannel: Name, sessChan: Name): SessionTypingEnvironment = {
      //println("enterJoin: " + ste)
      val (globalModel, role) = delegator.getGlobalTypeForChannel(sharedChannel)
      val projectedModel = project(globalModel, role)
      new InProcessEnv(
        delegator.ste.updated(sessChan, new Session(typeSystem, projectedModel)), 
        delegator, role, sessChan, infEnv
      )
    }

    override def enterThen(delegator: SessionTypingEnvironment) = new ThenBlockEnv(delegator.ste, delegator)

    def updated(ste: SessionTypedElements) = {
      assert(ste.sessions.values.map(_.isComplete).foldRight(true)(_&&_),
        "Top-level env should only be updated with completed sessions. ste: " + ste)
      new ProcessBlocksPassTopLevelEnv(ste, infEnv)
    }

    override def send(sessChan: Name, role: String, msgSig: MsgSig, delegator: SessionTypingEnvironment) = notYet("send")
    override def receive(sessChan: Name, role: String, msgSig: MsgSig, delegator: SessionTypingEnvironment) = notYet("receive")

    override def enterChoiceReceiveBlock(delegator: SessionTypingEnvironment, sessChan: Name, srcRole: Option[String]) = notYet("choice receive")
    override def leaveChoiceReceiveBlock = notLeavingYet("choice receive")

    override def enterChoiceReceiveBranch(srcRole: Option[String], msgSig: MsgSig) = notYet("choice receive branch")
    override def leaveChoiceReceiveBranch = notLeavingYet("choice receive branch")

    override def enterSessionMethod(delegator: SessionTypingEnvironment, fun: Symbol, sessChans: List[Name]) =
      new WaitForJoinEnv(delegator.ste, delegator)
  }

  /** To properly check bind blocks contained inside session methods. */
  class WaitForJoinEnv(val ste: SessionTypedElements, parent: SessionTypingEnvironment) extends AbstractTopLevelEnv(parent) {
    override def leaveSessionMethod(returnedChans: List[Name]) = parent
    def updated(ste: SessionTypedElements) = new WaitForJoinEnv(ste, parent)

    override def enterJoin(delegator: SessionTypingEnvironment, sharedChannel: Name, sessChan: Name) =
      parent.enterJoin(delegator, sharedChannel, sessChan)
  }

  class RecoverableTypeCheckingException(msg: String, val recoveryEnv: SessionTypingEnvironment)
          extends SessionTypeCheckingException(msg)

  class InProcessEnv(val ste: SessionTypedElements,
                     parent: SessionTypingEnvironment,
                     joinAsRole: Role,
                     sessChanJoin: Name,
                     infEnv: InferredTypeRegistry)
  extends AbstractDelegatingEnv(parent) {
    //println("Created InProcessEnv: " + ste)

    override def leaveJoin: SessionTypingEnvironment = {
      // .toSet to make it an immutable.Set, silly API fixed in scala svn:
      // http://lampsvn.epfl.ch/trac/scala/ticket/4001, can be removed after scala 2.8.2 is released
      val scopedChans = ste.sessions.keySet.toSet -- parent.ste.sessions.keySet
      val scopedSessions = scopedChans map (c => (c, ste.sessions(c)))

      val newSte = ste.removeSessions(scopedChans)
      //println("leave join: " + joinAsRole + ", ste: " + ste + ", newSte: " + newSte)
      val newEnv = parent.updated(newSte)

      for ((chan, session) <- scopedSessions) {
        if (!session.isComplete)
          throw new RecoverableTypeCheckingException("Session not completed on channel "
                + chan + " for role " + joinAsRole + ", remaining activities: " + session.remaining,
            newEnv)
      }
      newEnv
    }

    override def isSessionChannel(ident: Name) = {
      ste.hasSessionChannel(ident) || parent.isSessionChannel(ident)
    }

    def updated(ste: SessionTypedElements) =
      new InProcessEnv(ste, parent, joinAsRole, sessChanJoin, infEnv)

    override def send(sessChan: Name, dstRoleName: String, msgSig: MsgSig, delegator: SessionTypingEnvironment): SessionTypingEnvironment = {
      val sess = delegator.ste.sessions(sessChan)
      val newSess = sess.interaction(
        joinAsRole, new Role(dstRoleName), msgSig.toScribble)
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

    override def enterChoiceReceiveBlock(delegator: SessionTypingEnvironment, sessChan: Name, srcRole: Option[String]) =
      new ChoiceReceiveBlockEnv(delegator.ste, delegator, sessChan, srcRole, Nil, None)

    def retrieveInferred(method: Symbol, delegatedChans: List[Name], returnedChans: List[Name]):
    Seq[(Name, Int, RecBlock, Option[Name])] =
    {
      def eval[P,R](f: PartialFunction[P,R], x: P): Option[R] =
        if (f.isDefinedAt(x)) Some(f(x)) else None
      val chansWithIndex = delegatedChans.zipWithIndex
      val chansWithRetIndexOpt = for ((c,i) <- chansWithIndex)
        yield (c, infEnv.returnRank(method, i))
      
      val chansWithRetChanOpt = for ((c, retIndexOpt) <- chansWithRetIndexOpt) 
        yield (c, 
               for (retIndex <- retIndexOpt; retChan <- eval(returnedChans, retIndex)) 
                   yield retChan)
      
      for (((chan, retChanOption), index) <- chansWithRetChanOpt.zipWithIndex) 
        yield (chan, index, infEnv.inferredSessionType(method, index), retChanOption)
      
      // result: List[(chan, chanRank, recur, retChanOpt)] with retChanOpt the correct name
      // corresponding to chan. The remaining session type for chan will be transferred
      // to retChanOpt (if defined).
    }
    override def delegation(delegator: SessionTypingEnvironment, method: Symbol, delegatedChans: List[Name], returnedChans: List[Name]): SessionTypingEnvironment = {
      val inferred = retrieveInferred(method, delegatedChans, returnedChans)
      //println("INFERRED:   " + inferred)
      val updated = (inferred foldLeft delegator) {
        case (env, (chan, chanRank, recur, retChanOpt)) =>
          val sess = env.ste.sessions(chan)
          val advancedSession = advanceOne(sess, recur, Nil)
          retChanOpt match {
            case Some(retChan) =>
              env.updated(retChan, advancedSession).updated(chan, finished(sess))
            case None =>
              env.updated(chan, advancedSession)
          }
      }
      // FrozenChannelsEnv is necessary only for bad programs where the delegation
      // did not complete the session, but the channel was not returned
      // and bound to a new value to be completed after the method call
      new FrozenChannelsEnv(updated.ste, updated, delegatedChans.iterator, Nil.iterator,
        "cannot be used anymore in this scope after it has been passed as a method parameter")
    }

    def finished(baseSess: Session) = new Session(baseSess, List[Activity]() asJava)

    def advanceList(sess: Session, acts: Seq[Activity], replacedLabels: List[String]): Session =
      (acts foldLeft sess) (advanceOne(_, _, replacedLabels))

    def advanceOne(sess: Session, act: Activity, replacedLabels: List[String]): Session = act match {
      case i: Interaction => sendOrReceive(sess, i)
      case c: Choice =>
        val src = c.getFromRole
        val dst = c.getToRole
        // we iterate on c's branches, so this supports sending less branches than specified for a choice send
        c.getWhens foreach { infWhen =>
          val sessBranch = sess.findMatchingWhen(src, dst, infWhen)
          advanceList(sessBranch, infWhen.getBlock.getContents.asScala, replacedLabels)
        }
        sess.dropFirst
      case r: RecBlock =>
        val expectedRecur = sess.getRecur
        if (expectedRecur != null) { // genuine expected recursion in spec
          // don't unroll, just jump into recur contents (otherwise infinite loop).
          val renamedSpec = alphaRename(contents(expectedRecur).asScala, expectedRecur.getLabel, r.getLabel)
          advanceList(new Session(sess, renamedSpec.asJava),
            contents(r).asScala, r.getLabel :: replacedLabels)
          // finally drop labelled block from session, as contents were checked without error
          sess.dropFirst
        } else { // this is an unnecessary inferred recursion, following the general inference scheme
          advanceList(sess, unroll(r), replacedLabels)
        }
      case r: Recursion =>
        //println("Recursion: " + r.getLabel + ", replacedLabels: " + replacedLabels)
        if (replacedLabels contains r.getLabel) sess.dropMatchingRecursionLabel(r)
        else {
          val recur = infEnv.inferredSessionType(r.getLabel)
          //println("inferred for " + r.getLabel + ": " + recur)
          advanceOne(sess, recur, replacedLabels)
        }
    }

    def contents(r: RecBlock) = r.getBlock.getContents

    // Similar to normal typechecking, this can be a choice selection as well as a normal interaction
    def sendOrReceive(sess: Session, i: Interaction) = {
      val msig = i.getMessageSignature
      val src = i.getFromRole
      val dsts = i.getToRoles
      val dst = if (dsts.isEmpty) null else dsts.get(0)
      //println("sendOrReceive - " + i + ", msig: " + msig)
      sess.interaction(src, dst, msig)
    }

    override def branchComplete(parentSte: SessionTypedElements, chan: Name, branch1: SessionTypedElements, branch2: SessionTypedElements, label: MsgSig) = {
      checkSessionsRemainingSame(branch1.sessions, branch2.sessions)
      branch1
    }

    override def returnStatement(delegator: SessionTypingEnvironment) = illegalReturn()
  }

  case class ChoiceReceiveBlockEnv(ste: SessionTypedElements,
                              override val parent: SessionTypingEnvironment,
                              chanChoice: Name,
                              choiceSrc: Option[String],
                              branches: List[MsgSig],
                              lastBranchSte: Option[SessionTypedElements])
  extends AbstractDelegatingEnv(parent) {

    //println("Created ChoiceReceiveBlockEnv: " + ste.sessions)

    def parentSession = ste.sessions(chanChoice)

    override def enterChoiceReceiveBranch(srcRole: Option[String], msgSig: MsgSig) = {
      // The choice activity at the beginning
      // of parentSession will only be removed when all branches
      // have been visited and correctly typechecked.
      val labelSignature = msgSig.toScribble
      val branchSrc = choiceSrc.orElse(srcRole).get
      println("enterChoiceReceiveBranch, src:"+branchSrc+", sig:"+labelSignature)
      val sessBranch = parentSession.branchReceive(
        labelSignature,
        new Role(branchSrc))
      val newSte = ste.updated(chanChoice, sessBranch)

      val updatedThis = copy(branches = msgSig :: branches)
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
      parentSession.checkBranchesSeen(
        branches map (l => l.toScribble)
        asJava
      )

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