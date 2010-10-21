package uk.ac.ic.doc.sessionscala.compiler

import org.scribble.common.logging.Journal
import org.scribble.protocol.projection.impl.ProtocolProjectorImpl
import org.scribble.protocol.model._
import java.util.{List => JList}
import tools.nsc.Global
import tools.nsc.util.BatchSourceFile
import scalaj.collection.Imports._
    
trait SessionTypingEnvironments {
  val scribbleJournal: Journal
  val global: Global
  import global._

  val projector = new ProtocolProjectorImpl

  object ScalaTypeSystem extends HostTypeSystem {
    lazy val rootCtx = global.analyzer.rootContext(
        new CompilationUnit(new BatchSourceFile("<sessiontyping>", "")))

    def scalaToScribble(t: Type): TypeReference = {
      val s = t.toString // fixme: temporary hack
      val i = s.indexOf('(')
      val end = if (i > 0) i else s.length
      val lastDot = s.lastIndexOf('.')
      var substring = s.substring(lastDot + 1, end)
      if (substring.equals("type")) {
        val before = s.substring(0, lastDot)
        substring = before.substring(before.lastIndexOf('.') + 1, before.length)
      }
      println("Converted Scala type: " + t + " to Scribble type: " + substring)
      new TypeReference(substring)
    }

    def scribbleToScala(imports: Seq[ImportList], tref: TypeReference): Type = {
      val found: Seq[Type] = imports.map({i: ImportList =>
        val javaPackage = i.getLocation
        assert(javaPackage != null)

        val typeImport: TypeImport = i.getTypeImport(tref.getName)
        if (typeImport != null) {
          val dataType = typeImport.getDataType
          assert(dataType.getFormat == "java")
          val javaClassName = dataType.getDetails
          Some(definitions.getClass(javaPackage + "." + javaClassName).tpe)
        } else None
      }).flatten
      if (found.length == 1) found(0)
      else if (found.length > 2) throw new IllegalStateException(
        "Should not happen: found more than 1 matching import for " + tref)
      else {
        val l = rootCtx.imports.map(_.importedSymbol(newTypeName(tref.getName)))
                .filter(_ != NoSymbol)
        if (l.length >= 1) l(0).tpe // order: scala.Predef, scala, java.lang
        else throw new SessionTypeCheckingException("Could not find pre-defined type: " + tref.getName)
      }
    }

    def isSubtype(subtype: TypeReference, supertype: TypeReference, imports: JList[ImportList]) =
      isSubType(scribbleToScala(imports.asScala, subtype),
                scribbleToScala(imports.asScala, supertype))

  }

  val typeSystem = ScalaTypeSystem

  type SharedChannels = Map[Name, ProtocolModel]
  type Sessions = Map[Name, Session]
  type Inferred = Map[Symbol, Map[Name, List[Activity]]]
  
  case class SessionTypedElements(sharedChannels: SharedChannels, sessions: Sessions, inferred: Inferred) {
    def this() = this(Map(), Map(), Map())
    def updated(sessChan: Name, newSess: Session): SessionTypedElements =
      new SessionTypedElements(
        sharedChannels, sessions.updated(sessChan, newSess), inferred)

    def updated(sharedChan: Name, model: ProtocolModel): SessionTypedElements =
      new SessionTypedElements(
        sharedChannels.updated(sharedChan, model), sessions, inferred)

    def updated(newSess: Sessions) = SessionTypedElements(sharedChannels, newSess, inferred)

    def updated(method: Symbol, inf: Map[Name, List[Activity]]): SessionTypedElements = 
      SessionTypedElements(sharedChannels, sessions, inferred.updated(method, inf))
    
    def getSharedChan(name: Name) = sharedChannels.get(name)

    def getInferredFor(method: Symbol, chan: Name): List[Activity] = 
      getInferredFor(method).get(chan).getOrElse(Nil)
    def getInferredFor(method: Symbol): Map[Name, List[Activity]] = 
      inferred.get(method).getOrElse(Map())
    def append(method: Symbol, chan: Name, act: Activity) = 
      updated(method, chan, getInferredFor(method, chan) ++ List(act))  
	def updated(method: Symbol, chan: Name, inferred: List[Activity]): SessionTypedElements = 
	  updated(method, getInferredFor(method).updated(chan, inferred))
  }

  def createInteraction(src: Role, dst: Role, msgType: TypeReference) =
    new Interaction(src, dst, new MessageSignature(msgType))
  
  def createWhen(label: TypeReference): When = {
    val w = new When
    w.setMessageSignature(new MessageSignature(label))
	w.setBlock(new org.scribble.protocol.model.Block)
    w
  }
  def createWhen(label: TypeReference, block: List[Activity]): When = {
	val w = createWhen(label)
	w.getBlock().getContents().addAll(block asJava)
	w
  }
  
  def createChoice(src: Role, dst: Role, branches: List[TypeReference]): Choice = {
    val c = new Choice
    c.setFromRole(src)
    c.setToRole(dst)
    c.getWhens().addAll((branches map (createWhen(_))) asJava)
    c
  }
  def createChoice(src: Role, label: TypeReference, block: List[Activity]): Choice = {
	val c = new Choice
	c.setFromRole(src)
	c.getWhens.add(createWhen(label, block))
	c
  }
  def addToChoice(c: Choice, w: When) = {
	val newC = new Choice
    newC.setFromRole(c.getFromRole)
    c.getWhens foreach (newC.getWhens.add(_))
	newC.getWhens.add(w)
	newC
  }
  
  def createRecur(label: String, block: List[Activity]) = {
    val r = new Recur
    r.setLabel(label)
    block foreach (r.getBlock().add(_))
    r
  }
  
  def createRecursion(label: String) = {
    val r = new Recursion
    r.setLabel(label)
    r
  }
  
  def checkSessionsRemainingSame(sessions1: Sessions, sessions2: Sessions): Unit = sessions1 foreach {
    case (chan, sessElse) =>
      val sessThen = sessions2(chan)
      if (sessElse.remaining != sessThen.remaining)
        throw new SessionTypeCheckingException(
          "Branch statement did not advance session equally on all branches on channel: "
                  + chan + ". A branch had remaining session type: "
                  + sessThen.remaining + " while another had: " + sessElse.remaining)    
  }

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
    
    def send(sessChan: Name, role: String, msgType: Type): SessionTypingEnvironment =
      send(sessChan, role, msgType, this)
    def send(sessChan: Name, role: String, msgType: Type, delegator: SessionTypingEnvironment): SessionTypingEnvironment = this
    def receive(sessChan: Name, role: String, msgType: Type): SessionTypingEnvironment =
      receive(sessChan, role, msgType, this)
    def receive(sessChan: Name, role: String, msgType: Type, delegator: SessionTypingEnvironment): SessionTypingEnvironment = this
    
    def enterChoiceReceiveBlock(delegator: SessionTypingEnvironment, sessChan: Name, srcRole: String): SessionTypingEnvironment = this
    def enterChoiceReceiveBlock(sessChan: Name, srcRole: String): SessionTypingEnvironment =
      enterChoiceReceiveBlock(this, sessChan, srcRole)
 
    def enterChoiceReceiveBranch(labelType: Type): SessionTypingEnvironment = this
    def leaveChoiceReceiveBranch: SessionTypingEnvironment = this 
    def leaveChoiceReceiveBlock: SessionTypingEnvironment = this 
    
    def enterThen: SessionTypingEnvironment = enterThen(this)
    def enterThen(delegator: SessionTypingEnvironment): SessionTypingEnvironment = this
    def enterElse: SessionTypingEnvironment = this
    def leaveIf: SessionTypingEnvironment = this

    def delegation(function: Symbol, channels: List[Name]): SessionTypingEnvironment =
      delegation(this, function, channels)
    def delegation(delegator: SessionTypingEnvironment, function: Symbol, channels: List[Name]): SessionTypingEnvironment = this
  
    def enterSessionMethod(fun: Symbol, sessChans: List[Name]): SessionTypingEnvironment = this
    def leaveSessionMethod: SessionTypingEnvironment = this
    
    def branchComplete(chan: Name, branch1: SessionTypedElements, branch2: SessionTypedElements, label: Type): SessionTypedElements = throw new IllegalStateException
  }
  
  abstract class AbstractDelegatingEnv(val parent: SessionTypingEnvironment) 
  extends SessionTypingEnvironment {
    def isSessionChannel(c: Name) = parent.isSessionChannel(c)
    
    def registerSharedChannel(name: Name, globalType: ProtocolModel, delegator: SessionTypingEnvironment): SessionTypingEnvironment =
      parent.registerSharedChannel(name, globalType, delegator)
    
    override def enterJoin(delegator: SessionTypingEnvironment, sharedChannel: Name, roleName: String, sessChan: Name) =
      parent.enterJoin(delegator, sharedChannel, roleName, sessChan)
    override def leaveJoin = parent.leaveJoin
    
    override def send(sessChan: Name, role: String, msgType: Type, delegator: SessionTypingEnvironment): SessionTypingEnvironment =
      parent.send(sessChan, role, msgType, delegator)
    
    override def receive(sessChan: Name, role: String, msgType: Type, delegator: SessionTypingEnvironment) =
      parent.receive(sessChan, role, msgType, delegator)
    
    override def enterChoiceReceiveBlock(delegator: SessionTypingEnvironment, sessChan: Name, srcRole: String) =
      parent.enterChoiceReceiveBlock(delegator, sessChan, srcRole)
    override def enterChoiceReceiveBranch(labelType: Type) =
      parent.enterChoiceReceiveBranch(labelType)
    override def leaveChoiceReceiveBranch = parent.leaveChoiceReceiveBranch
    override def leaveChoiceReceiveBlock = parent.leaveChoiceReceiveBlock
    
    override def enterThen(delegator: SessionTypingEnvironment) = parent.enterThen(delegator)
    override def enterElse = parent.enterElse
    override def leaveIf = parent.leaveIf

    override def delegation(delegator: SessionTypingEnvironment, function: Symbol, channels: List[Name]) =
      parent.delegation(delegator, function, channels)
    
    override def branchComplete(chan: Name, branch1: SessionTypedElements, branch2: SessionTypedElements, label: Type) = 
      parent.branchComplete(chan, branch1, branch2, label)
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
  
  class MethodSessionTypeInferenceTopLevelEnv(val ste: SessionTypedElements) extends AbstractTopLevelEnv {
    def this() = this(new SessionTypedElements)
    
    def registerSharedChannel(name: Name, globalType: ProtocolModel, delegator: SessionTypingEnvironment): SessionTypingEnvironment =
      throw new IllegalStateException("Should not be called")
    
    def updated(ste: SessionTypedElements) = 
      new MethodSessionTypeInferenceTopLevelEnv(ste)
    
    override def enterJoin(delegator: SessionTypingEnvironment, sharedChannel: Name, roleName: String, sessChan: Name): SessionTypingEnvironment =
      throw new IllegalStateException("Should not be called")
    
    override def enterSessionMethod(fun: Symbol, sessChans: List[Name]): SessionTypingEnvironment = {
      println("enterSessionMethod")
      new InMethodInferenceEnv(this, ste, fun, sessChans)
    }
    
    def inferredSessionType(method: Symbol, chan: Name): Recur = 
      createRecur(method.encodedName, ste.getInferredFor(method, chan))
      
  }
  
  class InMethodInferenceEnv(parent: SessionTypingEnvironment, val ste: SessionTypedElements, 
      method: Symbol, chans: List[Name]) extends AbstractDelegatingEnv(parent){
    def updated(newSte: SessionTypedElements) =
      new InMethodInferenceEnv(parent, newSte, method, chans)
    override def isSessionChannel(chan: Name) = chans.contains(chan)
    
    def inferInteraction(chan: Name, inter: Interaction, delegator: SessionTypingEnvironment) = {
      val dSte = delegator.ste
      val newSte = dSte.append(method, chan, inter)
      delegator.updated(newSte)
    }
    
    override def send(sessChan: Name, role: String, msgType: Type, delegator: SessionTypingEnvironment) = {
      val inter = createInteraction(null, new Role(role), new TypeReference(typeSystem.scalaToScribble(msgType)))
      println("send, delegator: " + delegator)
      inferInteraction(sessChan, inter, delegator)
    }
      
    override def receive(sessChan: Name, role: String, msgType: Type, delegator: SessionTypingEnvironment) = {
      val inter = createInteraction(new Role(role), null, new TypeReference(typeSystem.scalaToScribble(msgType)))
      println("receive, delegator: " + delegator)
      inferInteraction(sessChan, inter, delegator)
    }

    override def enterChoiceReceiveBlock(delegator: SessionTypingEnvironment, sessChan: Name, srcRole: String) =
      new InfChoiceReceiveBlockEnv(delegator.ste, delegator, method, sessChan, srcRole, None)

    override def enterThen(delegator: SessionTypingEnvironment) = new ThenBlockEnv(delegator.ste, delegator)

    override def leaveSessionMethod = parent.updated(ste)

    override def branchComplete(chan: Name, withChoice: SessionTypedElements, toMerge: SessionTypedElements, labelToMerge: Type) = {
	  // todo: check Other Sessions Inferred same or subtype, and keep most general
	  assert(withChoice.getInferredFor(method, chan).length == 1)
	  val choice = withChoice.getInferredFor(method, chan)(0).asInstanceOf[Choice]
	  val block = toMerge.getInferredFor(method, chan)
	  val w = createWhen(typeSystem.scalaToScribble(labelToMerge), block)
	  withChoice.updated(method, chan, List(addToChoice(choice, w)))
	}
  }
  
  class JoinBlockTopLevelEnv(val ste: SessionTypedElements, val infEnv: SessionTypingEnvironment) extends AbstractTopLevelEnv {
    def this() = this(new SessionTypedElements, null)
    def this(ste: SessionTypedElements) = this(ste, null)
    def this(infEnv: SessionTypingEnvironment) = this(new SessionTypedElements, infEnv)

    def registerSharedChannel(name: Name, globalType: ProtocolModel, delegator: SessionTypingEnvironment): SessionTypingEnvironment =
      delegator.updated(delegator.ste.updated(name, globalType))
    
    override def enterJoin(delegator: SessionTypingEnvironment, sharedChannel: Name, roleName: String, sessChan: Name): SessionTypingEnvironment = {
      //println("enterJoin: " + ste)
      val role = new Role(roleName)
      val globalModel = delegator.getGlobalTypeForChannel(sharedChannel)
      val projectedModel = projector.project(globalModel, role, scribbleJournal)
      new InProcessEnv(
        delegator.ste.updated(sessChan, new Session(typeSystem, projectedModel)),
        delegator, role, sessChan)
    }
          
    def updated(ste: SessionTypedElements) = {
      assert(ste.sessions.values.map(_.isComplete).foldRight(true)(_&&_))
      new JoinBlockTopLevelEnv(ste)
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

    override def delegation(delegator: SessionTypingEnvironment, function: Symbol, channels: List[Name]): SessionTypingEnvironment = {
      // todo: new env that forbids any use of s (delegated)
      delegator
    }
    
    override def branchComplete(chan: Name, branch1: SessionTypedElements, branch2: SessionTypedElements, label: Type) = {
      checkSessionsRemainingSame(branch1.sessions, branch2.sessions)
      branch1
    }
  }

  class InProcessEnv(val ste: SessionTypedElements,
                     parent: SessionTypingEnvironment,
                     joinAsRole: Role,
                     sessChanJoin: Name)
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
      new InProcessEnv(ste, parent, joinAsRole, sessChanJoin)

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
        newSte.updated(chanChoice, parentSession.choiceChecked))
    }
  }

  class InfChoiceReceiveBlockEnv(val ste: SessionTypedElements,
                              parent: SessionTypingEnvironment,
							  method: Symbol,
                              chanChoice: Name,
                              choiceSrc: String,
                              lastBranchSte: Option[SessionTypedElements])
  extends AbstractDelegatingEnv(parent) {

    override def enterChoiceReceiveBranch(label: Type) = {
	  // create a fresh inferred list for the new branch
	  // this will then be merged with lastBranchSte in InfChoiceReceiveBranchEnv.leaveChoiceReceiveBranch
	  val newSte = ste.updated(method, chanChoice, Nil)
      new InfChoiceReceiveBranchEnv(newSte, this, method, choiceSrc, chanChoice, label, lastBranchSte)
    }

    def updated(ste: SessionTypedElements): SessionTypingEnvironment = updated(ste, lastBranchSte)
    def updated(ste: SessionTypedElements, lbste: Option[SessionTypedElements]): SessionTypingEnvironment =
      new InfChoiceReceiveBlockEnv(ste, parent, method, chanChoice, choiceSrc, lbste)
	def withLastBranchSte(lbste: SessionTypedElements) =
	  updated(ste, Some(lbste))

    override def leaveChoiceReceiveBlock = {
	  // the Choice object was created by successive branchComplete calls in the BranchEnvs earlier, and is now in lastBranchSte

      // to keep advance of interleaved sessions on other channels than chanChoice
      val newSte = lastBranchSte.get
      // lastBranchSte.get.getInferredFor(method, chanChoice) only contains the branch, nothing before
      // now we replace it by the parent inferred list, appending the choice
	  val choice = newSte.getInferredFor(method, chanChoice)(0) 
	  val parentInferred = parent.ste.getInferredFor(method, chanChoice)
      parent.updated(newSte.updated(method, chanChoice, parentInferred ++ List(choice)))
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
        branchComplete(chanChoice, lastBranchSte.get, ste, branchLabel)
      } else {
		val freshChoice = createChoice(new Role(choiceSrc), typeSystem.scalaToScribble(branchLabel), 
			ste.getInferredFor(method, chanChoice))
		ste.append(method, chanChoice, freshChoice)
	  }

      parent.withLastBranchSte(mergedSte)
    }

    def updated(ste: SessionTypedElements) =
      new InfChoiceReceiveBranchEnv(ste, parent, method, choiceSrc, chanChoice, branchLabel, lastBranchSte)
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
        branchComplete(chanChoice, ste, lastBranchSte.get, branchLabel)
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

  class ElseBlockEnv(val ste: SessionTypedElements,
                     parent: SessionTypingEnvironment,
                     steThenBranch: SessionTypedElements)
      extends AbstractDelegatingEnv(parent) {

    //println("Created ElseBlockEnv: " + ste + " sessionsThenBranch: "
    //        + sessionsThenBranch + ", parent.ste.sessions: " + parent.ste.sessions)

    override def leaveIf = {
      parent.updated(branchComplete(null, ste, steThenBranch, null))
    }

    def updated(newSte: SessionTypedElements) =
      new ElseBlockEnv(newSte, parent, steThenBranch)
  }
}
