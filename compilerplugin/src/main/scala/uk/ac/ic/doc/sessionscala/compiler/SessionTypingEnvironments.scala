package uk.ac.ic.doc.sessionscala.compiler

import org.scribble.common.logging.Journal
import org.scribble.protocol.projection.impl.ProtocolProjectorImpl
import org.scribble.protocol.model._
import java.util.{List => JList}
import tools.nsc.Global
import tools.nsc.util.BatchSourceFile

trait SessionTypingEnvironments {
  val scribbleJournal: Journal
  val global: Global
  import global._

  lazy val rootCtx = global.analyzer.rootContext(
    new CompilationUnit(new BatchSourceFile("<sessiontyping>", "")))

  val projector = new ProtocolProjectorImpl

  class ScalaTypeSystem extends HostTypeSystem {
    def scalaToScribble(t: Type): TypeReference = {
      val s = t.toString // fixme: temporary hack
      val i = s.indexOf('(')
      val end = if (i > 0) i else s.length
      val substring = s.substring(s.lastIndexOf('.') + 1, end)
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

    import scalaj.collection.Imports._
    def isSubtype(subtype: TypeReference, supertype: TypeReference, imports: JList[ImportList]) =
      isSubType(scribbleToScala(imports.asScala, subtype),
                scribbleToScala(imports.asScala, supertype))

  }

  val typeSystem = new ScalaTypeSystem

  type SharedChannels = Map[Name, ProtocolModel]
  type Sessions = Map[Name, Session]

  def createInteraction(src: Role, dst: Role, msgType: TypeReference) =
    new Interaction(src, dst, new MessageSignature(msgType))

  trait SessionTypingEnvironment {

    val sharedChannels: SharedChannels
    val sessions: Sessions
    val parent: SessionTypingEnvironment

    val protoModel = new ProtocolModel
    val protocol = new Protocol
    protoModel.setProtocol(protocol)

    protected def createInstance(sharedChannels: SharedChannels, sessions: Sessions): SessionTypingEnvironment

    def isSessionChannel(c: Name) = false

    def registerSharedChannel(name: Name, globalType: ProtocolModel): SessionTypingEnvironment = {
      createInstance(sharedChannels + (name -> globalType), sessions)
    }

    def enterJoin(sharedChannel: Name, roleName: String, sessChan: Name): SessionTypingEnvironment = {
      println("enterJoin, sharedChannels: " + sharedChannels + ", sessions: " + sessions)
      val role = new Role(roleName)
      val globalModel = getGlobalTypeForChannel(sharedChannel)
      val projectedModel = projector.project(globalModel, role, scribbleJournal)
      new InProcessEnvironment(sharedChannels, this, role,
        sessChan, sessions + (sessChan -> new Session(typeSystem, projectedModel)))
    }

    def leaveJoin: SessionTypingEnvironment = {
      throw new SessionTypeCheckingException("trying to leave a join block, but was at top-level environment")
    }

    protected def getGlobalTypeForChannel(name: Name): ProtocolModel =
      sharedChannels.get(name).getOrElse(
        if (parent == null)
          throw new SessionTypeCheckingException("Channel: " + name + " is not in scope")
        else
          parent.getGlobalTypeForChannel(name)
        )    

    def send(sessChan: Name, role: String, msgType: Type): SessionTypingEnvironment = {
      throw new SessionTypeCheckingException("trying to do a send operation, but not in join block yet")
    }

    def receive(sessChan: Name, role: String, msgType: Type): SessionTypingEnvironment = {
      throw new SessionTypeCheckingException("trying to do a receive operation, but not in join block yet")
    }

    def enterBranchReceiveBlock(sessChan: Name, srcRole: String): SessionTypingEnvironment = this
    def enterIndividualBranchReceive(labelType: Type) = this
    def leaveIndividualBranchReceive = this
    def leaveBranchReceiveBlock = this

    def delegation(function: Symbol, channels: List[Name]): SessionTypingEnvironment = {
      // todo: new env that forbids any use of s (delegated)
      this
    }
  }

  class TopLevelSessionTypingEnvironment(val sharedChannels: SharedChannels) extends SessionTypingEnvironment {
    val parent: SessionTypingEnvironment = null
    val sessions: Sessions = Map.empty

    def this() = this(Map.empty)

    def createInstance(sharedChans: SharedChannels, sessions: Sessions): SessionTypingEnvironment = {
      assert(sessions.isEmpty);
      new TopLevelSessionTypingEnvironment(sharedChans)
    }
  }

  class InProcessEnvironment
  (sharedChans: SharedChannels, override val parent: SessionTypingEnvironment, role: Role, sessChanThisBlock: Name, override val sessions: Sessions)
          extends TopLevelSessionTypingEnvironment(sharedChans) {

    def session = sessions(sessChanThisBlock)
    override def leaveJoin: SessionTypingEnvironment = {
      println("leave join: " + role)

      if (!session.isComplete)
        throw new SessionTypeCheckingException(
          "Session not completed, remaining activities: " + session.remaining)

      parent
    }

    override def isSessionChannel(ident: Name): Boolean = {
      if (ident == sessChanThisBlock) true
      else parent.isSessionChannel(ident)
    }

    override def createInstance(newSharedChans: SharedChannels, newSessions: Sessions): SessionTypingEnvironment = {
      new InProcessEnvironment(newSharedChans, parent, role, sessChanThisBlock, newSessions)
    }

    override def send(sessChan: Name, dstRoleName: String, msgType: Type): SessionTypingEnvironment = {
      val sess = sessions(sessChan)
      val dstRole = new Role(dstRoleName)

      val newSess = sess.interaction(
        role, dstRole, typeSystem.scalaToScribble(msgType))
      println(
        "send: on " + sessChan + " from " + role + " to " +
        dstRole + ": " + msgType + ". Updated session: " + newSess)
      updated(sessChan, newSess)

    }

    override def receive(sessChan: Name, srcRole: String, msgType: Type): SessionTypingEnvironment = {
      val sess = sessions(sessChan)

      val newSess = sess.interaction(
        new Role(srcRole), role, typeSystem.scalaToScribble(msgType))
      println(
        "receive: on " + sessChan + " from " + srcRole + " to " +
        role + ": " + msgType + ". Updated session: " + newSess)
      updated(sessChan, newSess)
    }

    def updated(sessChan: Name, newSess: Session) =
      createInstance(sharedChannels, sessions.updated(sessChan, newSess))
  }

  class InBranchEnvironment(sharedChans: SharedChannels, parent: SessionTypingEnvironment, role: Role, sessChan: Name, branchLabel: Type, sessions: Sessions)
          extends InProcessEnvironment(sharedChans, parent, role, sessChan, sessions) {
    override def leaveIndividualBranchReceive = {
      println("leave branch: " + branchLabel)
      // Todo: check session type is completed
      parent
    }

    override def leaveJoin: SessionTypingEnvironment = {
      error("Branch not finished, but leaving join block")
      parent
    }

    override def createInstance(newSharedChans: SharedChannels, newSessions: Sessions): SessionTypingEnvironment = {
      new InBranchEnvironment(newSharedChans, parent, role, sessChan, branchLabel, newSessions)
    }
  }
}