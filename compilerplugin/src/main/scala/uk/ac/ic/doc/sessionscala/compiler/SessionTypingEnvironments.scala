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

    def updated(method: Symbol, inf: Map[Name, List[Activity]]) = 
      SessionTypedElements(sharedChannels, sessions, inferred.updated(method, inf))
    
    def getSharedChan(name: Name) = sharedChannels.get(name)
    def getInferredFor(method: Symbol, chan: Name): List[Activity] = 
      getInferredFor(method).get(chan).getOrElse(Nil)
    def getInferredFor(method: Symbol): Map[Name, List[Activity]] = 
      inferred.get(method).getOrElse(Map())
    def appendInferred(method: Symbol, chan: Name, act: Activity) = {
      val appended = getInferredFor(method, chan) ++ List(act)
      updated(method, getInferredFor(method).updated(chan, appended))  
    }   
  }

  def createInteraction(src: Role, dst: Role, msgType: TypeReference) =
    new Interaction(src, dst, new MessageSignature(msgType))
  
  def createWhen(label: TypeReference): When = {
    val w = new When
    w.setMessageSignature(new MessageSignature(label))
    w
  }
  
  def createChoice(src: Role, dst: Role, branches: List[TypeReference]) = {
    val c = new Choice
    c.setFromRole(src)
    c.setToRole(dst)
    c.getWhens().addAll((branches map (createWhen(_))) asJava)
    c
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
  
  def checkSessionsIdentical(sessions1: Sessions, sessions2: Sessions): Unit = sessions1 foreach {
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
    def enterJoin(delegator: SessionTypingEnvironment, sharedChannel: Name, roleName: String, sessChan: Name): SessionTypingEnvironment
    def leaveJoin: SessionTypingEnvironment
    
    def getGlobalTypeForChannel(name: Name): ProtocolModel =
      ste.getSharedChan(name).getOrElse(
        if (parent == null)
          throw new SessionTypeCheckingException("Channel: " + name + " is not in scope")
        else
          parent.getGlobalTypeForChannel(name))    
    
    def send(sessChan: Name, role: String, msgType: Type): SessionTypingEnvironment =
      send(sessChan, role, msgType, this)
    def send(sessChan: Name, role: String, msgType: Type, delegator: SessionTypingEnvironment): SessionTypingEnvironment
    def receive(sessChan: Name, role: String, msgType: Type): SessionTypingEnvironment =
      receive(sessChan, role, msgType, this)
    def receive(sessChan: Name, role: String, msgType: Type, delegator: SessionTypingEnvironment): SessionTypingEnvironment
    
    def enterChoiceReceiveBlock(delegator: SessionTypingEnvironment, sessChan: Name, srcRole: String): SessionTypingEnvironment
    def enterChoiceReceiveBlock(sessChan: Name, srcRole: String): SessionTypingEnvironment =
      enterChoiceReceiveBlock(this, sessChan, srcRole)
 
    def enterChoiceReceiveBranch(labelType: Type): SessionTypingEnvironment
    def leaveChoiceReceiveBranch: SessionTypingEnvironment
    def leaveChoiceReceiveBlock: SessionTypingEnvironment
    
    def enterThen: SessionTypingEnvironment = enterThen(this)
    def enterThen(delegator: SessionTypingEnvironment): SessionTypingEnvironment
    def enterElse: SessionTypingEnvironment
    def leaveIf: SessionTypingEnvironment

    def delegation(function: Symbol, channels: List[Name]): SessionTypingEnvironment =
      delegation(this, function, channels)
    def delegation(delegator: SessionTypingEnvironment, function: Symbol, channels: List[Name]): SessionTypingEnvironment
  
    def enterSessionMethod(fun: Symbol, sessChans: List[Name]): SessionTypingEnvironment = this
    def leaveSessionMethod: SessionTypingEnvironment = this
    
    def branchComplete(branch1: SessionTypedElements, branch2: SessionTypedElements): SessionTypedElements
    
  }
  
  abstract class AbstractDelegatingEnv(val parent: SessionTypingEnvironment) 
  extends SessionTypingEnvironment {
    def isSessionChannel(c: Name) = parent.isSessionChannel(c)
    
    def registerSharedChannel(name: Name, globalType: ProtocolModel, delegator: SessionTypingEnvironment): SessionTypingEnvironment =
      parent.registerSharedChannel(name, globalType, delegator)
    
    def enterJoin(delegator: SessionTypingEnvironment, sharedChannel: Name, roleName: String, sessChan: Name) =
      parent.enterJoin(delegator, sharedChannel, roleName, sessChan)
    def leaveJoin = parent.leaveJoin
    
    def send(sessChan: Name, role: String, msgType: Type, delegator: SessionTypingEnvironment): SessionTypingEnvironment =
      parent.send(sessChan, role, msgType, delegator)
    
    def receive(sessChan: Name, role: String, msgType: Type, delegator: SessionTypingEnvironment) =
      parent.receive(sessChan, role, msgType, delegator)
    
    def enterChoiceReceiveBlock(delegator: SessionTypingEnvironment, sessChan: Name, srcRole: String) =
      parent.enterChoiceReceiveBlock(delegator, sessChan, srcRole)
    def enterChoiceReceiveBranch(labelType: Type) =
      parent.enterChoiceReceiveBranch(labelType)
    def leaveChoiceReceiveBranch = parent.leaveChoiceReceiveBranch
    def leaveChoiceReceiveBlock = parent.leaveChoiceReceiveBlock
    
    def enterThen(delegator: SessionTypingEnvironment) = parent.enterThen(delegator)
    def enterElse = parent.enterElse
    def leaveIf = parent.leaveIf

    def delegation(delegator: SessionTypingEnvironment, function: Symbol, channels: List[Name]) =
      parent.delegation(delegator, function, channels)
    
    def branchComplete(branch1: SessionTypedElements, branch2: SessionTypedElements) = 
      parent.branchComplete(branch1, branch2)
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
    
    def enterJoin(delegator: SessionTypingEnvironment, sharedChannel: Name, roleName: String, sessChan: Name): SessionTypingEnvironment =
      throw new IllegalStateException("Should not be called")
    
    def leaveJoin = this

    def send(sessChan: Name, role: String, msgType: Type, delegator: SessionTypingEnvironment) = this
    def receive(sessChan: Name, role: String, msgType: Type, delegator: SessionTypingEnvironment) = this

    def enterChoiceReceiveBlock(delegator: SessionTypingEnvironment, sessChan: Name, srcRole: String) = this
    def leaveChoiceReceiveBlock = this

    def enterChoiceReceiveBranch(labelType: Type) = this
    def leaveChoiceReceiveBranch = this

    def enterThen(delegator: SessionTypingEnvironment): SessionTypingEnvironment = this
    def enterElse: SessionTypingEnvironment = this
    def leaveIf: SessionTypingEnvironment = this

    def delegation(delegator: SessionTypingEnvironment, function: Symbol, channels: List[Name]) = this    

    override def enterSessionMethod(fun: Symbol, sessChans: List[Name]): SessionTypingEnvironment = {
      println("enterSessionMethod")
      new InMethodInferenceEnv(this, ste, fun, sessChans)
    }
    
    def inferredSessionType(method: Symbol, chan: Name): List[Activity] = 
      ste.getInferredFor(method, chan)
      
    def branchComplete(branch1: SessionTypedElements, branch2: SessionTypedElements) = branch1 // todo: fuse/add to inferred  
  }
  
  class InMethodInferenceEnv(parent: SessionTypingEnvironment, val ste: SessionTypedElements, 
      method: Symbol, chans: List[Name]) extends AbstractDelegatingEnv(parent){
    def updated(newSte: SessionTypedElements) =
      new InMethodInferenceEnv(parent, newSte, method, chans)
    override def isSessionChannel(chan: Name) = chans.contains(chan)
    
    def inferInteraction(chan: Name, inter: Interaction, delegator: SessionTypingEnvironment) = {
      val dSte = delegator.ste
      val newSte = dSte.appendInferred(method, chan, inter)
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

    override def leaveSessionMethod = parent.updated(ste)
  }
  
  class JoinBlockTopLevelEnv(val ste: SessionTypedElements, val infEnv: SessionTypingEnvironment) extends AbstractTopLevelEnv {
    def this() = this(new SessionTypedElements, null)
    def this(ste: SessionTypedElements) = this(ste, null)
    def this(infEnv: SessionTypingEnvironment) = this(new SessionTypedElements, infEnv)

    def registerSharedChannel(name: Name, globalType: ProtocolModel, delegator: SessionTypingEnvironment): SessionTypingEnvironment =
      delegator.updated(delegator.ste.updated(name, globalType))
    
    def enterJoin(delegator: SessionTypingEnvironment, sharedChannel: Name, roleName: String, sessChan: Name): SessionTypingEnvironment = {
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
    
    def leaveJoin: SessionTypingEnvironment = notLeavingYet("join")

    def send(sessChan: Name, role: String, msgType: Type, delegator: SessionTypingEnvironment) = notYet("send")
    def receive(sessChan: Name, role: String, msgType: Type, delegator: SessionTypingEnvironment) = notYet("receive")

    def enterChoiceReceiveBlock(delegator: SessionTypingEnvironment, sessChan: Name, srcRole: String) = notYet("choice receive")
    def leaveChoiceReceiveBlock = notLeavingYet("choice receive")

    def enterChoiceReceiveBranch(labelType: Type) = notYet("choice receive branch")
    def leaveChoiceReceiveBranch = notLeavingYet("choice receive branch")

    def enterThen(delegator: SessionTypingEnvironment): SessionTypingEnvironment = notYet("then branch")
    def enterElse: SessionTypingEnvironment = notYet("else branch")
    def leaveIf: SessionTypingEnvironment = notLeavingYet("if")

    def delegation(delegator: SessionTypingEnvironment, function: Symbol, channels: List[Name]): SessionTypingEnvironment = {
      // todo: new env that forbids any use of s (delegated)
      delegator
    }
    
    def branchComplete(branch1: SessionTypedElements, branch2: SessionTypedElements) = {
      checkSessionsIdentical(branch1.sessions, branch2.sessions)
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
                              sessChanReceiveBlock: Name,
                              choiceSrcRole: String,
                              branches: List[Type],
                              lastBranchSte: Option[SessionTypedElements])
  extends AbstractDelegatingEnv(parent) {

    //println("Created ChoiceReceiveBlockEnv: " + ste.sessions)

    def parentSession = ste.sessions(sessChanReceiveBlock)

    override def enterChoiceReceiveBranch(label: Type) = {
      // The choice activity at the beginning
      // of parentSession will only be removed when all branches
      // have been visited and correctly typechecked.
      val labelSignature = new MessageSignature(typeSystem.scalaToScribble(label))
      val sessBranch = parentSession.visitBranch(
        labelSignature,
        new Role(choiceSrcRole))
      val newSte = ste.updated(sessChanReceiveBlock, sessBranch)

      val updatedThis = new ChoiceReceiveBlockEnv(ste, parent, sessChanReceiveBlock,
        choiceSrcRole, label :: branches, lastBranchSte)
      new ChoiceReceiveBranchEnv(newSte, updatedThis, sessChanReceiveBlock, 
          branches, label, lastBranchSte)
    }

    def updated(ste: SessionTypedElements) =
      new ChoiceReceiveBlockEnv(ste, parent, sessChanReceiveBlock, 
          choiceSrcRole, branches, lastBranchSte)

    def withLastBranchSte(lastBranchSte: SessionTypedElements): ChoiceReceiveBlockEnv = {
      new ChoiceReceiveBlockEnv(ste, parent, sessChanReceiveBlock, 
          choiceSrcRole, branches, Some(lastBranchSte))
    }

    override def leaveChoiceReceiveBlock = {
      //println("seen branches: " + branches)
      val missing = parentSession.missingBranches(
        branches map (l => new MessageSignature(typeSystem.scalaToScribble(l)))
        asJava
      )
      if (!missing.isEmpty)
        throw new SessionTypeCheckingException("Missing choice receive branch(es): " + missing)

      // to keep advance of interleaved sessions on other channels than sessChanReceiveBlock
      val newSte = lastBranchSte.get
      // lastBranchSte.sessions.get(sessChanReceiveBlock) is empty as it only had the branch block
      // now we replace it by the parent session which still had the whole thing,
      // only removing the choice construct at the beginning
      parent.updated(
        newSte.updated(sessChanReceiveBlock, parentSession.choiceChecked))
    }
  }

  class ChoiceReceiveBranchEnv(val ste: SessionTypedElements,
                               parent: ChoiceReceiveBlockEnv,
                               sessChanReceiveBlock: Name,
                               branches: List[Type],
                               branchLabel: Type,
                               lastBranchSte: Option[SessionTypedElements])
          extends AbstractDelegatingEnv(parent) {

    //println("Created ChoiceReceiveBranchEnv: " + ste.sessions)

    override def leaveChoiceReceiveBranch = {
      //println("leave branch: " + branchLabel)
      val sess = ste.sessions(sessChanReceiveBlock)
      if (!sess.isComplete) // sess only has the branch block, not what comes after it
        throw new SessionTypeCheckingException("Branch incomplete, missing: "+ sess.remaining)
      
      val nextSte = if (lastBranchSte.isDefined) {
        branchComplete(ste, lastBranchSte.get)
        //println("checked last branch ok: " + lastBranchSess.get)
      } else ste

      parent.withLastBranchSte(nextSte)
    }

    def updated(ste: SessionTypedElements) =
      new ChoiceReceiveBranchEnv(ste, parent, sessChanReceiveBlock, 
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
      parent.updated(branchComplete(ste, steThenBranch))
    }

    def updated(newSte: SessionTypedElements) =
      new ElseBlockEnv(newSte, parent, steThenBranch)
  }
}