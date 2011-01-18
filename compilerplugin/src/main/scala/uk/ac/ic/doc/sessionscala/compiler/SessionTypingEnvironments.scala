package uk.ac.ic.doc.sessionscala.compiler

import org.scribble.common.logging.Journal
import org.scribble.protocol.projection.impl.ProtocolProjectorImpl
import org.scribble.protocol.model._
import tools.nsc.Global
import scalaj.collection.Imports._

trait SessionTypingEnvironments extends InferenceEnvironments with CommonEnvironments {
  self: ScalaTypeSystemComponent with ScribbleModelFactories
          with SessionTypedElementsComponent =>

  val scribbleJournal: Journal
  val global: Global
  import global.{Block => _, _}

  def checkSessionsRemainingSame(sessions1: Sessions, sessions2: Sessions): Unit = sessions1 foreach {
    case (chan, sessElse) =>
      val sessThen = sessions2(chan)
      if (sessElse.remaining != sessThen.remaining)
        throw new SessionTypeCheckingException(branchesUneven + " On channel: "
                  + chan + ", a branch had remaining session type: "
                  + sessThen.remaining + " while another had: " + sessElse.remaining)    
  }

  class JoinBlockTopLevelEnv(val ste: SessionTypedElements, val infEnv: InferredTypeRegistry) extends AbstractTopLevelEnv {
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
      assert(ste.sessions.values.map(_.isComplete).foldRight(true)(_&&_))
      new JoinBlockTopLevelEnv(ste, infEnv)
    }
    
    override def leaveJoin: SessionTypingEnvironment = notLeavingYet("join")

    override def send(sessChan: Name, role: String, msgType: Type, delegator: SessionTypingEnvironment) = notYet("send")
    override def receive(sessChan: Name, role: String, msgType: Type, delegator: SessionTypingEnvironment) = notYet("receive")

    override def enterChoiceReceiveBlock(delegator: SessionTypingEnvironment, sessChan: Name, srcRole: String) = notYet("choice receive")
    override def leaveChoiceReceiveBlock = notLeavingYet("choice receive")

    override def enterChoiceReceiveBranch(labelType: Type) = notYet("choice receive branch")
    override def leaveChoiceReceiveBranch = notLeavingYet("choice receive branch")

    override def enterThen(delegator: SessionTypingEnvironment): SessionTypingEnvironment = notYet("then branch")
    override def enterElse: SessionTypingEnvironment = notYet("else branch")
    override def leaveIf: SessionTypingEnvironment = notLeavingYet("if")
  }

  def unroll(recur: LabelledBlock): Seq[Activity] = {
    def unrollRec(act: Activity): Activity = act match {
      case r: LabelledBlock if r.getLabel == recur.getLabel => r // masking
      case r: LabelledBlock => createLabelledBlock(r.getLabel, unrollRec(r.getBlock).asInstanceOf[Block])
      case rec: Recursion if rec.getLabel == recur.getLabel => recur
      case c: Choice => createChoice(c, (c.getWhens.asScala map (w => createWhen(w.getMessageSignature, unrollRec(w.getBlock).asInstanceOf[Block]))))
      case b: Block => createBlock(b.getContents.asScala.map(unrollRec(_)))
      case other => other
    }
    
    recur.getBlock.getContents.asScala.map(unrollRec(_))    
  }

  def alphaRename(acts: Seq[Activity], oldLabel: String, newLabel: String): Seq[Activity] = { 
    def alphaRenameRec(act: Activity): Activity = act match {
      case r: LabelledBlock if r.getLabel == oldLabel => r // masking
      case r: LabelledBlock => createLabelledBlock(r.getLabel, alphaRenameRec(r.getBlock).asInstanceOf[Block])
      case rec: Recursion if rec.getLabel == oldLabel => createRecursion(newLabel)
      case c: Choice => createChoice(c, (c.getWhens.asScala map (w => createWhen(w.getMessageSignature, alphaRenameRec(w.getBlock).asInstanceOf[Block]))))
      case b: Block => createBlock(b.getContents.asScala.map(alphaRenameRec(_)))
      case other => other
    }
    
    acts.map(alphaRenameRec(_))
  }

  /*
  class FrozenChannelsEnv(parent: SessionTypingEnvironment, channels: List[Name]) extends AbstractDelegatingEnv(parent) {

  }
  */

  class InProcessEnv(val ste: SessionTypedElements,
                     parent: SessionTypingEnvironment,
                     joinAsRole: Role,
                     sessChanJoin: Name,
                     infEnv: InferredTypeRegistry)
  extends AbstractDelegatingEnv(parent) {
    //println("Created InProcessEnv: " + ste)

    override def leaveJoin: SessionTypingEnvironment = {
      val session = ste.sessions(sessChanJoin)
      //println("leave join: " + joinAsRole)

      if (!session.isComplete)
        throw new SessionTypeCheckingException(
          "Session not completed, remaining activities: " + session.remaining)

      parent.updated(ste)
    }

    override def isSessionChannel(ident: Name) = {
      if (ident == sessChanJoin) true
      else parent.isSessionChannel(ident)
    }

    def updated(ste: SessionTypedElements) =
      new InProcessEnv(ste, parent, joinAsRole, sessChanJoin, infEnv)

    override def send(sessChan: Name, dstRoleName: String, msgType: Type, delegator: SessionTypingEnvironment): SessionTypingEnvironment = {
      val sess = delegator.ste.sessions(sessChan)
      val dstRole = new Role(dstRoleName)

      val newSess = sess.interaction(
        joinAsRole, dstRole, typeSystem.scalaToScribble(msgType))
      /*println(
        "send: on " + sessChan + " from " + joinAsRole + " to " +
        dstRole + ": " + msgType + ". Updated session: " + newSess
                + ", parent.ste.sessions: " + parent.ste.sessions + ", class: " + getClass())*/
      delegator.updated(sessChan, newSess)
    }

    override def receive(sessChan: Name, srcRole: String, msgType: Type, delegator: SessionTypingEnvironment): SessionTypingEnvironment = {
      val sess = delegator.ste.sessions(sessChan)

      val newSess = sess.interaction(
        new Role(srcRole), joinAsRole, typeSystem.scalaToScribble(msgType))
      /*println(
        "receive: on " + sessChan + " from " + srcRole + " to " +
        joinAsRole + ": " + msgType + ". Updated session: " + newSess)*/
      delegator.updated(sessChan, newSess)
    }

    override def enterChoiceReceiveBlock(delegator: SessionTypingEnvironment, sessChan: Name, srcRole: String) =
      new ChoiceReceiveBlockEnv(delegator.ste, delegator, sessChan, srcRole, Nil, None)

    override def enterThen(delegator: SessionTypingEnvironment) = new ThenBlockEnv(delegator.ste, delegator)

    override def delegation(delegator: SessionTypingEnvironment, method: Symbol, channels: List[Name]): SessionTypingEnvironment = {
      val inferred = channels map {c =>
        (c, infEnv.inferredSessionType(method, c))
      }
      println(inferred)
      val updated = (inferred foldLeft delegator) {
        case (env, (chan, recur)) =>
          val sess = env.ste.sessions(chan)
          env.updated(chan, advanceOne(chan, sess, recur, List()))
      }
      // todo: new env that forbids any use of s (delegated) (forbid send/receive/delegation, others still ok)
      //new FrozenChannelsEnv(updated, channels)
      updated
    }

    def advanceList(chan: Name, sess: Session, acts: Seq[Activity], replaced: List[String]): Session =
      (acts foldLeft sess) (advanceOne(chan, _, _, replaced))

    def advanceOne(chan: Name, sess: Session, act: Activity, replaced: List[String]): Session = act match {
      case i: Interaction => sendOrReceive(sess, i)
      case c: Choice =>
        /*
        if (isChoiceReceive(c)) {
          // todo: it's legal to have more branches than specified in a choice receive        
        } else {	      
	      } */
        val src = c.getFromRole
        val dst = c.getToRole
        c.getWhens foreach { infWhen => // we iterate on c's branches, so this supports sending less branches than specified for a choice send
          val sessBranch = sess.findMatchingWhen(src, dst, infWhen)
          advanceList(chan, sessBranch, infWhen.getBlock.getContents.asScala, replaced)
        }
        sess.dropFirst
      case r: LabelledBlock => 
        val expectedRecur = sess.getRecur
        if (expectedRecur != null) { // genuine expected recursion in spec
          // don't unroll, just jump into recur contents (otherwise infinite loop)
          val renamedSpec = alphaRename(contents(expectedRecur).asScala, expectedRecur.getLabel, r.getLabel)
          advanceList(chan, new Session(sess, renamedSpec.asJava), contents(r).asScala, r.getLabel :: replaced)
          sess.dropFirst
        } else { // unnecessary inferred recursion, following general inference scheme
          advanceList(chan, sess, unroll(r), replaced)
        }
      case r: Recursion =>
        println("Recursion: " + r.getLabel + ", replaced: " + replaced)
        if (replaced contains r.getLabel) sess.recursionLabel(r)
        else {
          val method = infEnv.methodFor(r.getLabel)
          assert(notEmpty(infEnv.inferredSessionType(method, chan)), "Calling method: "
                + method + ": No inferred session type for channel: " + chan + " in env: " + infEnv)
          val recur = infEnv.inferredSessionType(method, chan)
          println("inferred for " + method + ", chan: " + chan + ": " + recur)
          advanceOne(chan, sess, recur, replaced)
        }
    }
  
    def contents(r: LabelledBlock) = r.getBlock.getContents

    // Similar to normal typechecking, this can be a choice selection as well as a normal interaction
    def sendOrReceive(sess: Session, i: Interaction) = {
      val tRef = i.getMessageSignature.getTypeReferences.get(0)
      val src = i.getFromRole
      val dsts = i.getToRoles
      val dst = if (dsts.isEmpty) null else dsts.get(0)
      println("sendOrReceive - " + i + ", tRef: " + tRef)
      sess.interaction(src, dst, tRef)
    }

    override def branchComplete(parentSte: SessionTypedElements, chan: Name, branch1: SessionTypedElements, branch2: SessionTypedElements, label: Type) = {
      checkSessionsRemainingSame(branch1.sessions, branch2.sessions)
      branch1
    }
  }

  class ChoiceReceiveBlockEnv(val ste: SessionTypedElements,
                              parent: SessionTypingEnvironment,
                              chanChoice: Name,
                              choiceSrc: String,
                              branches: List[Type],
                              lastBranchSte: Option[SessionTypedElements])
  extends AbstractDelegatingEnv(parent) {

    //println("Created ChoiceReceiveBlockEnv: " + ste.sessions)

    def parentSession = ste.sessions(chanChoice)

    override def enterChoiceReceiveBranch(label: Type) = {
      // The choice activity at the beginning
      // of parentSession will only be removed when all branches
      // have been visited and correctly typechecked.
      val labelSignature = new MessageSignature(typeSystem.scalaToScribble(label))
      val sessBranch = parentSession.visitBranch(
        labelSignature,
        new Role(choiceSrc))
      val newSte = ste.updated(chanChoice, sessBranch)

      val updatedThis = new ChoiceReceiveBlockEnv(ste, parent, chanChoice,
        choiceSrc, label :: branches, lastBranchSte)
      new ChoiceReceiveBranchEnv(newSte, updatedThis, chanChoice, 
          branches, label, lastBranchSte)
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
        branches map (l => new MessageSignature(typeSystem.scalaToScribble(l)))
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
                               branches: List[Type],
                               branchLabel: Type,
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
