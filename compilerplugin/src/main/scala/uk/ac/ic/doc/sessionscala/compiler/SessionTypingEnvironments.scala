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

  lazy val rootCtx = global.analyzer.rootContext(
    new CompilationUnit(new BatchSourceFile("<sessiontyping>", "")))

  val projector = new ProtocolProjectorImpl

  object ScalaTypeSystem extends HostTypeSystem {
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

  case class SessionTypedElements(sharedChannels: SharedChannels, sessions: Sessions) {
    def updated(sessChan: Name, newSess: Session): SessionTypedElements =
      new SessionTypedElements(
        sharedChannels, sessions.updated(sessChan, newSess))

    def updated(sharedChan: Name, model: ProtocolModel): SessionTypedElements =
      new SessionTypedElements(
        sharedChannels.updated(sharedChan, model), sessions)

    def getSharedChan(name: Name) = sharedChannels.get(name)
  }

  def createInteraction(src: Role, dst: Role, msgType: TypeReference) =
    new Interaction(src, dst, new MessageSignature(msgType))

  trait SessionTypingEnvironment {

    val ste: SessionTypedElements
    val parent: SessionTypingEnvironment

    val protoModel = new ProtocolModel
    val protocol = new Protocol
    protoModel.setProtocol(protocol)

    def createInstance(ste: SessionTypedElements): SessionTypingEnvironment

    def isSessionChannel(c: Name) = false

    def registerSharedChannel(name: Name, globalType: ProtocolModel): SessionTypingEnvironment = {
      createInstance(ste.updated(name, globalType))
    }

    def enterJoin(sharedChannel: Name, roleName: String, sessChan: Name): SessionTypingEnvironment = {
      println("enterJoin: " + ste)
      val role = new Role(roleName)
      val globalModel = getGlobalTypeForChannel(sharedChannel)
      val projectedModel = projector.project(globalModel, role, scribbleJournal)
      new InProcessEnvironment(
        ste.updated(sessChan, new Session(typeSystem, projectedModel)),
        this, role, sessChan)
    }

    def leaveJoin: SessionTypingEnvironment = notLeavingYet("join")

    protected def getGlobalTypeForChannel(name: Name): ProtocolModel =
      ste.getSharedChan(name).getOrElse(
        if (parent == null)
          throw new SessionTypeCheckingException("Channel: " + name + " is not in scope")
        else
          parent.getGlobalTypeForChannel(name)
        )    

    def notYet(what: String) =
      throw new SessionTypeCheckingException("trying to do a " + what
              + " operation, but not in appropriate block yet")
    def notLeavingYet(what: String) =
      throw new SessionTypeCheckingException("trying to leave a " + what
              + " block, but was not yet in such a block environment")


    def send(sessChan: Name, role: String, msgType: Type): SessionTypingEnvironment =
      notYet("send")

    def receive(sessChan: Name, role: String, msgType: Type): SessionTypingEnvironment =
      notYet("receive")

    def enterBranchReceiveBlock(sessChan: Name, srcRole: String): SessionTypingEnvironment =
      notYet("choice receive")
    def leaveBranchReceiveBlock: SessionTypingEnvironment =
      notLeavingYet("choice receive")

    def enterIndividualBranchReceive(labelType: Type): SessionTypingEnvironment =
      notYet("choice receive branch")
    def leaveIndividualBranchReceive: SessionTypingEnvironment =
      notLeavingYet("choice receive branch")

    def delegation(function: Symbol, channels: List[Name]): SessionTypingEnvironment = {
      // todo: new env that forbids any use of s (delegated)
      this
    }

    def updated(sessChan: Name, newSess: Session) =
      createInstance(ste.updated(sessChan, newSess))

  }

  class TopLevelSessionTypingEnvironment(val ste: SessionTypedElements)
          extends SessionTypingEnvironment {
    
    val parent: SessionTypingEnvironment = null

    def this() = this(new SessionTypedElements(Map.empty, Map.empty))

    def createInstance(ste: SessionTypedElements): SessionTypingEnvironment = {
      assert(ste.sessions.values.map(_.isComplete).foldRight(true)(_&&_))
      new TopLevelSessionTypingEnvironment(ste)
    }
  }

  class InProcessEnvironment(ste: SessionTypedElements,
                             parent: SessionTypingEnvironment,
                             joinAsRole: Role,
                             sessChanJoin: Name)
          extends TopLevelSessionTypingEnvironment(ste) {

    def session = ste.sessions(sessChanJoin)
    override def leaveJoin: SessionTypingEnvironment = {
      println("leave join: " + joinAsRole)

      if (!session.isComplete)
        throw new SessionTypeCheckingException(
          "Session not completed, remaining activities: " + session.remaining)

      parent.createInstance(ste)
    }

    override def isSessionChannel(ident: Name): Boolean = {
      if (ident == sessChanJoin) true
      else parent.isSessionChannel(ident)
    }

    override def createInstance(ste: SessionTypedElements): SessionTypingEnvironment = {
      new InProcessEnvironment(ste, parent, joinAsRole, sessChanJoin)
    }

    override def send(sessChan: Name, dstRoleName: String, msgType: Type): SessionTypingEnvironment = {
      val sess = ste.sessions(sessChan)
      val dstRole = new Role(dstRoleName)

      val newSess = sess.interaction(
        joinAsRole, dstRole, typeSystem.scalaToScribble(msgType))
      println(
        "send: on " + sessChan + " from " + joinAsRole + " to " +
        dstRole + ": " + msgType + ". Updated session: " + newSess)
      updated(sessChan, newSess)

    }

    override def receive(sessChan: Name, srcRole: String, msgType: Type): SessionTypingEnvironment = {
      val sess = ste.sessions(sessChan)

      val newSess = sess.interaction(
        new Role(srcRole), joinAsRole, typeSystem.scalaToScribble(msgType))
      println(
        "receive: on " + sessChan + " from " + srcRole + " to " +
        joinAsRole + ": " + msgType + ". Updated session: " + newSess)
      updated(sessChan, newSess)
    }

    override def enterBranchReceiveBlock(sessChan: Name, srcRole: String) = {
      new InChoiceReceiveBlockEnvironment(ste, this, joinAsRole, sessChanJoin,
        sessChan, srcRole, Nil)
    }

  }

  class InChoiceReceiveBlockEnvironment(ste: SessionTypedElements,
                                        parent: SessionTypingEnvironment,
                                        joinAsRole: Role,
                                        sessChanJoin: Name,
                                        sessChanReceiveBlock: Name,
                                        choiceSrcRole: String,
                                        branches: List[Type])
          extends InProcessEnvironment(ste, parent, joinAsRole, sessChanJoin) {

    def parentSession = ste.sessions(sessChanReceiveBlock)

    override def enterIndividualBranchReceive(label: Type) = {
      // The choice activity at the beginning
      // of parentSession will only be removed when all branches
      // have been visited and correctly typechecked.
      val labelSignature = new MessageSignature(typeSystem.scalaToScribble(label))
      val sessBranch = parentSession.visitBranch(
        labelSignature,
        new Role(choiceSrcRole))
      val newSte = ste.updated(sessChanReceiveBlock, sessBranch)

      val updatedThis = new InChoiceReceiveBlockEnvironment(
        ste, parent, joinAsRole, sessChanJoin, sessChanReceiveBlock,
        choiceSrcRole, label :: branches)
      new InChoiceReceiveBranchEnvironment(newSte, updatedThis, joinAsRole,
        sessChanJoin, sessChanReceiveBlock, choiceSrcRole, branches, label)
    }

    override def createInstance(ste: SessionTypedElements): SessionTypingEnvironment =
      new InChoiceReceiveBlockEnvironment(ste, parent, joinAsRole, sessChanJoin, sessChanReceiveBlock,
        choiceSrcRole, branches)

    override def leaveBranchReceiveBlock = {
      println("seen branches: " + branches)
      val missing = parentSession.missingBranches(
        branches map (l => new MessageSignature(typeSystem.scalaToScribble(l)))
        asJava
      )
      if (!missing.isEmpty)
        throw new SessionTypeCheckingException("Missing choice receive branch(es): " + missing)

      parent.updated(sessChanReceiveBlock, parentSession.choiceChecked)
    }
  }

  class InChoiceReceiveBranchEnvironment(ste: SessionTypedElements,
                                         parent: SessionTypingEnvironment,
                                         joinAsRole: Role,
                                         sessChanJoin: Name,
                                         sessChanReceiveBlock: Name,
                                         choiceSrcRole: String,
                                         branches: List[Type],
                                         branchLabel: Type)
          extends InChoiceReceiveBlockEnvironment(ste, parent, joinAsRole,
            sessChanJoin, sessChanReceiveBlock, choiceSrcRole, branches) {

    override def leaveIndividualBranchReceive = {
      println("leave branch: " + branchLabel)
      val sess = ste.sessions(sessChanReceiveBlock)
      if (!sess.isComplete)
        throw new SessionTypeCheckingException("Branch incomplete, missing: "+ sess.remaining)
      parent
    }

    override def leaveJoin = throw new IllegalStateException
    override def leaveBranchReceiveBlock = throw new IllegalStateException

    override def createInstance(ste: SessionTypedElements): SessionTypingEnvironment = {
      new InChoiceReceiveBranchEnvironment(ste, parent, joinAsRole,
        sessChanJoin, sessChanReceiveBlock, choiceSrcRole, branches, branchLabel)
    }
  }
}