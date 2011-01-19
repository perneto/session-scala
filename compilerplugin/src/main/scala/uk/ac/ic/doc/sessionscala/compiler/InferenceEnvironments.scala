package uk.ac.ic.doc.sessionscala.compiler

import tools.nsc.Global
import org.scribble.protocol.model._

/**
 * Created by: omp08
 */

trait InferenceEnvironments {
  self: SessionTypedElementsComponent with ScribbleModelFactories with CommonEnvironments with ScalaTypeSystemComponent =>

  val global: Global
  import global._

  def appendInferred(method: Symbol, parentSte: SessionTypedElements, branch: SessionTypedElements) =
    allInferred(method, parentSte, branch).foldLeft(parentSte) { (result, chan) =>
      result.appendAll(method, chan, branch.getInferredFor(method, chan))
    }

  def allInferred(method: Symbol, ste1: SessionTypedElements, ste2: SessionTypedElements): Iterable[Name] = {
    val inf1 = ste1.getInferredFor(method)
      val inf2 = ste2.getInferredFor(method)
      inf1.keys ++ inf2.keys
  }

  class MethodSessionTypeInferenceTopLevelEnv(val ste: SessionTypedElements) extends AbstractTopLevelEnv with InferredTypeRegistry {
    def this() = this(EmptySTE)

    def registerSharedChannel(name: Name, globalType: ProtocolModel, delegator: SessionTypingEnvironment): SessionTypingEnvironment =
      throw new IllegalStateException("Should not be called")

    def updated(ste: SessionTypedElements) =
      new MethodSessionTypeInferenceTopLevelEnv(ste)

    override def enterJoin(delegator: SessionTypingEnvironment, sharedChannel: Name, roleName: String, sessChan: Name): SessionTypingEnvironment =
      throw new IllegalStateException("Should not be called")

    override def enterSessionMethod(fun: Symbol, sessChans: List[Name]): SessionTypingEnvironment = {
      val newSte = (sessChans foldLeft ste) { case (ste, chan) =>
        ste.createInferred(fun, chan)
      }
      println("enterSessionMethod, ste: " + ste + ", newSte: " + newSte)
      new InMethodInferenceEnv(this, newSte, fun, sessChans)
    }

    def inferredSessionType(method: Symbol, chan: Name): LabelledBlock = {
      //println("inferredSessionType: " + method + ", chan: " + chan + ", inferred: " + ste.getInferredFor(method, chan) + ", ste: " + ste)
      ste.getInferred(method, chan) match {
        case Some(labelledBlock) => labelledBlock
        case None => throw new IllegalArgumentException("No inferred session type known for: " + method + " and channel: " + chan)
      }
    }

    def methodFor(label: String): Symbol = {
      println("methodFor: " + label + ", ste: " + ste)
      ste.methodFor(label).get
    }

    override def toString = "MethodSessionTypeInferenceTopLevelEnv{ste: " + ste + "}"
    override def inferred = this
  }

  class InMethodInferenceEnv(parent: SessionTypingEnvironment, val ste: SessionTypedElements,
      method: Symbol, chans: List[Name]) extends AbstractDelegatingEnv(parent) {

    def updated(newSte: SessionTypedElements) =
      new InMethodInferenceEnv(parent, newSte, method, chans)
    override def isSessionChannel(chan: Name) = chans.contains(chan)

    def inferInteraction(chan: Name, inter: Interaction, delegator: SessionTypingEnvironment) = {
      //println("inferInteraction, in: " + method + " on chan: " + chan + ": " + inter)
      val dSte = delegator.ste
      val newSte = dSte.append(method, chan, inter)
      delegator.updated(newSte)
    }

    override def send(sessChan: Name, role: String, msgType: Type, delegator: SessionTypingEnvironment) = {
      val inter = createInteraction(null, new Role(role), typeSystem.scalaToScribble(msgType))
      //println("send, delegator: " + delegator)
      inferInteraction(sessChan, inter, delegator)
    }

    override def receive(sessChan: Name, role: String, msgType: Type, delegator: SessionTypingEnvironment) = {
      val inter = createInteraction(new Role(role), null, typeSystem.scalaToScribble(msgType))
      //println("receive, delegator: " + delegator)
      inferInteraction(sessChan, inter, delegator)
    }

    override def delegation(delegator: SessionTypingEnvironment, function: Symbol, delegatedChans: List[Name], returnedChannels: List[Name]) = {
      println("delegation, calling method: " + function + ", delegatedChans: " + delegatedChans)
      val dSte = delegator.ste
      val newSte = delegatedChans.foldLeft(dSte) {(ste, chan) =>
        val (steRegistered, label) = ste.registerMethod(function)
        println("delegation, appending inferred recursion variable: " + label)
        steRegistered.append(method, chan, createRecursion(label))
      }
      println("delegation, final newSte: " + newSte)
      delegator.updated(newSte)
    }

    override def enterChoiceReceiveBlock(delegator: SessionTypingEnvironment, sessChan: Name, srcRole: String) =
      new InfChoiceReceiveBlockEnv(delegator.ste, delegator, method, sessChan, srcRole, None)

    // here we create a new ste so that each branch only gives out contents of the branch at the end
    // this makes merging the inferred branches easier in branchComplete
    override def enterThen(delegator: SessionTypingEnvironment) = new InfThenBlockEnv(ste.clearAllButLabels, delegator)

    override def leaveSessionMethod(returnedChans: List[Name]) = {
      val (newSte, label) = ste.registerMethod(method)
      val inf = newSte.getInferredFor(method) map { case (chan, listInf) =>
        (chan -> List(createLabelledBlock(label, listInf)))
      }
      parent.updated(newSte.updated(method, inf))
    }

    override def branchComplete(parentSte: SessionTypedElements, chan: Name, withChoice: SessionTypedElements, toMerge: SessionTypedElements, labelToMerge: Type) = {
      if (chan != null) {
        sessionBranches(parentSte, withChoice, toMerge, chan, labelToMerge)
      } else {
        val merged = ifBranches(withChoice, toMerge)
        appendInferred(method, parentSte, merged)
      }
    }

    def sessionBranches(parentSte: SessionTypedElements, branch1: SessionTypedElements, branch2: SessionTypedElements, chan: Name, labelToMerge: Type) = {
      // the first branch wraps its inferred list in a choice, and thereafter this function preserves this invariant
      assert(branch1.getInferredFor(method, chan).length == 1, branch1.getInferredFor(method, chan))
      val choice = branch1.getInferredFor(method, chan)(0).asInstanceOf[Choice]
      val block = branch2.getInferredFor(method, chan)
      val w = createWhen(typeSystem.scalaToScribble(labelToMerge), block)
      // we use the ifBranches method for checking the interleaved sessions.
      // for this to work we need to remove the branch channel to avoid confusion
      // with merged label sends (choice sends)
      val checkedMerged = ifBranches(branch1.dropChan(method, chan),  branch2.dropChan(method, chan))
      checkedMerged.append(method, chan, addToChoice(choice, w))
    }

    def ifBranches(thenBranch: SessionTypedElements, elseBranch: SessionTypedElements): SessionTypedElements = {
      //println("ifBranches, thenBranch: " + thenBranch + ", elseBranch: " + elseBranch)
      // would start the fold with EmptySTE, but need to preserve the labels map
      // from the branches. then branch passes its labels down to else branch,
      // so we start with else branch here
      allInferred(method, thenBranch, elseBranch).foldLeft(elseBranch.clearAllButLabels) { (result, chan) =>
        val inferredThen = thenBranch.getInferredFor(method, chan)
        val inferredElse = elseBranch.getInferredFor(method, chan)
        result.appendAll(method, chan, merge(chan, inferredThen, inferredElse))
      }
    }

    // assumes either act1 <: act2 or act2 <: act1
    def chooseSide(act1: Activity, act2: Activity) =
      if (isSubtype(act1,act2))
        if (isSend(act2)) act2 else act1
      else if (isSend(act1)) act1 else act2

    def subtypeExists(act1: Activity, act2: Activity) =
      isSubtype(act1,act2) || isSubtype(act2,act1)

    def isSend(act: Activity) = act.initiatorRoles.isEmpty

    def splitPrefix(acts1: LA, acts2: LA): (LA, LAA) = {
      val (common, different) = (acts1.zipAll(acts2,null,null)).foldLeft((Nil:LA, Nil:LAA)) {
        case ((common,different), pair@(act1, act2)) =>
          if (different.isEmpty)
            if (subtypeExists(act1, act2)) (chooseSide(act1, act2) :: common, different)
            else (common, pair :: different)
          else (common, pair :: different)
      }
      (common reverse, different reverse)
    }

    def merge(chan: Name, acts1: LA, acts2: LA): LA = {
      //println("merge, acts1: " + acts1 + ", acts2: " + acts2)
      val (common, different) = splitPrefix(acts1, acts2)
      //println("common: " + common + ", different: " + different)
      if (different.isEmpty) common
      else {
        val (act1, act2) = different.head
        val (rest1, rest2) = different.tail.unzip
        val choice = if (act1.isInstanceOf[Choice]) updateChoice(act1.asInstanceOf[Choice], act2, rest1, rest2)
                     else if (act2.isInstanceOf[Choice]) updateChoice(act2.asInstanceOf[Choice], act1, rest1, rest2)
                     else mergeAsChoice(chan, act1, act2, rest1, rest2)

        //println(choice)
        common ::: List(choice)
      }
    }

    def updateChoice(c: Choice, act: Activity, empty: LA, restAct: LA) = {
      assert(empty.isEmpty)
      val send = act.asInstanceOf[Interaction]
      assert(send.getFromRole == null)
      assert(send.getToRoles.size == 1)
      assert(send.getFromRole == c.getFromRole)
      assert(send.getToRoles.get(0) == c.getToRole)
      addToChoice(c, createWhen(send.getMessageSignature, restAct))
    }

    def mergeAsChoice(chan: Name, act1: Activity, act2: Activity, rest1: LA, rest2: LA) = {
      def throwUneven(goodSend: Interaction) = throw new SessionTypeCheckingException(
          branchesUneven + " On channel: " + chan +
          ", one branch was sending choice label: " + goodSend.getMessageSignature +
          " while the other branch was not doing any session operations.")

      //println("mergeAsChoice, act1: " + act1)
      val send1 = act1.asInstanceOf[Interaction]
      val send2 = act2.asInstanceOf[Interaction]

      if (send2 == null) throwUneven(send1)
      if (send1 == null) throwUneven(send2)
      assert(send1.getFromRole == null)
      assert(send2.getFromRole == null)
      assert(send1.getToRoles == send2.getToRoles)
      assert(send1.getToRoles.size == 1)
      createChoice(send1.getToRoles.get(0), List(mkBranch(send1, rest1), mkBranch(send2, rest2)))
    }

    def mkBranch(send: Interaction, rest: LA) = (send.getMessageSignature, rest takeWhile (_ != null))

    import java.util.LinkedList
    def isSubtype(act1: Activity, act2: Activity) = act1 != null && act2 != null &&
      Session.isSubtype(typeSystem, new LinkedList, act1, act2)
      // Imports not required during inference, as we use the ScalaTypeReference class
      // to store native Type instances, which already include their fully qualified name.
      // The imports list is only needed at typechecking time.
  }

  class InfChoiceReceiveBlockEnv(val ste: SessionTypedElements,
                              parent: SessionTypingEnvironment,
                              method: Symbol,
                              chanChoice: Name,
                              choiceSrc: String,
                              lastBranchSte: Option[SessionTypedElements])
  extends AbstractDelegatingEnv(parent) {

    override def enterChoiceReceiveBranch(label: Type) = {
      // create a fresh STE for the new branch
      // this will then be merged with lastBranchSte in InfChoiceReceiveBranchEnv.leaveChoiceReceiveBranch
      new InfChoiceReceiveBranchEnv(ste.clearAllButLabels, this, method, choiceSrc, chanChoice, label, lastBranchSte)
    }

    def updated(ste: SessionTypedElements): SessionTypingEnvironment =
      updated(ste, lastBranchSte)
    def updated(ste: SessionTypedElements, lbste: Option[SessionTypedElements]): SessionTypingEnvironment =
      new InfChoiceReceiveBlockEnv(ste, parent, method, chanChoice, choiceSrc, lbste)
    def withLastBranchSte(lbste: SessionTypedElements) =
      updated(ste, Some(lbste))

    override def leaveChoiceReceiveBlock = {
      // the Choice object was created by successive branchComplete calls in the BranchEnvs earlier, and is now in lastBranchSte
      // it contains merged inferred sessions for the body of the branch, but needs to
      // be connected with the inferred sessions prior to the branch, in parent.ste
      parent.updated(appendInferred(method, parent.ste, lastBranchSte.get))
    }
  }

  class InfChoiceReceiveBranchEnv(val ste: SessionTypedElements,
                               parent: InfChoiceReceiveBlockEnv,
                               method: Symbol,
                               choiceSrc: String,
                               chanChoice: Name,
                               branchLabel: Type,
                               lastBranchSte: Option[SessionTypedElements])
          extends AbstractDelegatingEnv(parent) {

    override def leaveChoiceReceiveBranch = {
      val mergedSte = if (lastBranchSte.isDefined) {
        branchComplete(parent.ste, chanChoice, lastBranchSte.get, ste, branchLabel)
      } else { // first branch
        val freshChoice = createChoice(new Role(choiceSrc),
          typeSystem.scalaToScribble(branchLabel), ste.getInferredFor(method, chanChoice))
        ste.updated(method, chanChoice, List(freshChoice)) // overwrite what we just wrapped in a choice
      }

      parent.withLastBranchSte(mergedSte)
    }

    def updated(ste: SessionTypedElements) =
      new InfChoiceReceiveBranchEnv(ste, parent, method, choiceSrc, chanChoice, branchLabel, lastBranchSte)
  }

  class InfThenBlockEnv(val ste: SessionTypedElements,
                        parent: SessionTypingEnvironment)
      extends AbstractDelegatingEnv(parent) {

    override def updated(newSte: SessionTypedElements) =
      new InfThenBlockEnv(newSte, parent)

    override def enterElse = {
      // only difference with ThenBlockEnv: ste.clearAllButLabels here
      new ElseBlockEnv(ste.clearAllButLabels, parent, ste)
    }
  }
}