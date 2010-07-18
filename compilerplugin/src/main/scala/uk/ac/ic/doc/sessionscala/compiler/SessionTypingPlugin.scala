package uk.ac.ic.doc.sessionscala.compiler

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import org.scribble.protocol.parser.antlr.ANTLRProtocolParser
import org.scribble.common.logging.ConsoleJournal

import java.io.File

trait SessionDefinitions {
  val global: Global
  import global._

  // Lazy because otherwise global is not set yet, so NPE trying to access definitions
  lazy val protocolAnnotation = definitions.getClass("uk.ac.ic.doc.sessionscala.protocol")
  lazy val sharedChannelTrait = definitions.getClass("uk.ac.ic.doc.sessionscala.SharedChannel")
  lazy val inputChannelTrait = definitions.getClass("scala.actors.InputChannel")
  lazy val channelTrait = definitions.getClass("scala.actors.Channel")
  lazy val acceptMethod = definitions.getMember(sharedChannelTrait, "accept")
  lazy val instanceOfMethod = definitions.getMember(definitions.getClass("scala.Any"), "asInstanceOf")
  lazy val qmarkMethod = definitions.getMember(inputChannelTrait, "$qmark")
  lazy val bangMethod = definitions.getMember(channelTrait, "$bang")
  lazy val receiveMethod = definitions.getMember(inputChannelTrait, "receive")
}


class SessionTypingPlugin(val global: Global) extends Plugin {
  val name = "sessiontyping"
  val description = "Multiparty Session Type checking for actors using the session-scala library"

  val sessionchecker = new SessionTypeChecker {
    val global = SessionTypingPlugin.this.global;
    val runsAfter = List[String]("refchecks");    
  }

  val components = List[PluginComponent](sessionchecker)

  abstract class SessionTypeChecker extends PluginComponent
                                    with SessionTypingEnvironments
                                    with SessionDefinitions {
    import global._;
    val scribbleJournal = new ConsoleJournal

    class SessionTypecheckingTraverser(unitPath: String) extends Traverser {

      val scribbleParser = new ANTLRProtocolParser
      
      var sessionEnvironment: SessionTypingEnvironment = new TopLevelSessionTypingEnvironment

      override def traverse(tree: Tree) {
        val sym = tree.symbol
        tree match {
          case ValDef(_,name,_,rhs)
          if sym.hasAnnotation(protocolAnnotation) && sym.tpe <:< sharedChannelTrait.tpe =>
            val annotInfo = sym.getAnnotation(protocolAnnotation).get
            val (filenameTree : Literal)::_ = annotInfo.args
            val filename = filenameTree.value.stringValue
            val is = new java.io.FileInputStream(new File(unitPath, filename))
            val globalModel = scribbleParser.parse(is, scribbleJournal)
            sessionEnvironment.registerSharedChannel(name, globalModel)
            traverse(rhs)

          case ValDef(_,_,_,_) if sym.hasAnnotation(protocolAnnotation) =>
            reporter.warning(tree.pos, "The @protocol annotation only has effect on SharedChannel instances")
            super.traverse(tree)
          
          case Apply(
                 Apply(Select(Ident(chanIdent), _), Literal(role)::Nil),
                 Function(ValDef(_,sessChan,_,_)::Nil, block)::Nil)
          if tree.symbol == acceptMethod =>
            println("accept: " + role + ", sessChan: " + sessChan)
            sessionEnvironment = sessionEnvironment.enterAccept(chanIdent, role.stringValue, sessChan)
            traverse(block)
            sessionEnvironment = sessionEnvironment.leaveAccept

          case Apply(Select(Apply(Select(Ident(session),_),Literal(role)::Nil),_),arg::Nil)
          if tree.symbol == bangMethod && sessionEnvironment.isSessionChannel(session) =>
            println("bangMethod, arg: " + arg + ", arg.tpe: " + arg.tpe + ", session: " + session + ", role: " + role)
            sessionEnvironment.send(session, role.stringValue, arg.tpe)
            traverse(arg)

          case TypeApply(Select(qm @ Select(Ident(session),_), _), _)
          if qm.symbol == qmarkMethod && sessionEnvironment.isSessionChannel(session) =>
            println("qmarkMethod, tree.tpe:" + tree.tpe + ", session: " + session)
            sessionEnvironment.receive(session, tree.tpe)
            super.traverse(tree)

          case Apply(TypeApply(Select(Ident(session),_),_),
                     Function(_,Match(_,cases))::Nil)
          if tree.symbol == receiveMethod && sessionEnvironment.isSessionChannel(session) =>
              println("receiveMethod, session: " + session
                      + ", cases: " + cases)
              cases foreach { c: CaseDef =>
                if (! c.guard.isEmpty) {
                  reporter.error(c.guard.pos, "Receive clauses on session channels (branching) do not support guards yet")
                } else {
                  def processBranch = {
                    sessionEnvironment = sessionEnvironment.enterBranch(c.pat.tpe)
                    traverse(c.body)
                    sessionEnvironment = sessionEnvironment.leaveBranch
                  }
                  c.pat match {
                    case Select(_,name) => processBranch
                    case Ident(name) => processBranch
                    case _ =>
                      reporter.error(c.pat.pos, "Receive clauses on session channels (branching) do not support complex patterns yet")
                  }

                }
              }

          case _ =>
            super.traverse(tree)
        }
      }
    }

    val phaseName = name
    def newPhase(_prev: Phase) = new SessionTypingPhase(_prev)

    class SessionTypingPhase(prev: Phase) extends StdPhase(prev) {
      override def name = SessionTypingPlugin.this.name
      println("Session Typing Plugin starting")
      def apply(unit: global.CompilationUnit): Unit = {
        val unitPath = new File(unit.source.path).getParent()
         
        global.treeBrowsers.create().browse(unit.body)
        val typeChecker = new SessionTypecheckingTraverser(unitPath)
        typeChecker(unit.body)
      }
    }
  }

}