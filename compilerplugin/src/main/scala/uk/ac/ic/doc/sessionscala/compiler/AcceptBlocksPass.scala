package uk.ac.ic.doc.sessionscala.compiler

import org.scribble.protocol.parser.antlr.ANTLRProtocolParser
import tools.nsc.plugins.PluginComponent
import tools.nsc.Phase
import java.io.{File, FileInputStream}

abstract class AcceptBlocksPass extends PluginComponent
                                  with SessionTypingEnvironments
                                  with SessionDefinitions {
  import global._

  class SessionTypecheckingTraverser(unitPath: String) extends Traverser {

    val scribbleParser = new ANTLRProtocolParser
    var sessionEnvironment: SessionTypingEnvironment = new TopLevelSessionTypingEnvironment

    def isSessionChannelIdent(tree: Tree): Boolean = getSessionChannelName(tree).isDefined

    def getSessionChannelName(tree: Tree): Option[Name] = tree match {
      case Ident(name) if sessionEnvironment.isSessionChannel(name) => Some(name)
      case _ => None
    }

    def getSessionChannels(args: List[Tree]): List[Name] =
      args.map(getSessionChannelName).flatten      

    def linearityError(lhs: Any, rhs: Tree) {
      reporter.error(rhs.pos, "Cannot assign " + rhs
        + " to " + lhs + ": aliasing of session channels is forbidden")
    }

    override def traverse(tree: Tree) {
      val sym = tree.symbol
      tree match {
        case ValDef(_,name,_,rhs)
        if sym.hasAnnotation(protocolAnnotation) && sym.tpe <:< sharedChannelTrait.tpe =>
          val annotInfo = sym.getAnnotation(protocolAnnotation).get
          val (filenameTree : Literal)::_ = annotInfo.args
          val filename = filenameTree.value.stringValue
          val is = new FileInputStream(new File(unitPath, filename))
          val globalModel = scribbleParser.parse(is, scribbleJournal)
          //todo: validate model
          sessionEnvironment.registerSharedChannel(name, globalModel)
          traverse(rhs)

        case ValDef(_,_,_,_) if sym.hasAnnotation(protocolAnnotation) =>
          reporter.warning(tree.pos, "The @protocol annotation only has an effect on SharedChannel instances")
          super.traverse(tree)

        case Apply(Apply(Select(Ident(chanIdent), _), Literal(role)::Nil),
                   Function(ValDef(_,sessChan,_,_)::Nil, block)::Nil)
        if tree.symbol == acceptMethod =>
          println("accept: " + role + ", sessChan: " + sessChan)
          sessionEnvironment = sessionEnvironment.enterAccept(chanIdent, role.stringValue, sessChan)
          traverse(block)
          sessionEnvironment = sessionEnvironment.leaveAccept

        case Apply(Select(Apply(Select(Ident(session),_),Literal(role)::Nil),_),arg::Nil)
        if tree.symbol == bangMethod && sessionEnvironment.isSessionChannel(session) =>
          println("bangMethod, arg: " + arg + ", arg.tpe: " + arg.tpe + ", session: " + session + ", role: " + role)
          sessionEnvironment = sessionEnvironment.send(session, role.stringValue, arg.tpe)
          traverse(arg)

        case TypeApply(Select(qm @ Select(Ident(session),_), _), _)
        if qm.symbol == qmarkMethod && sessionEnvironment.isSessionChannel(session) =>
          println("qmarkMethod, tree.tpe:" + tree.tpe + ", session: " + session)
          sessionEnvironment = sessionEnvironment.receive(session, tree.tpe)
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

        case Apply(fun,args) if !getSessionChannels(args).isEmpty =>
          //todo: forbid delegation to methods starting another thread. require annotation?
          // starting another thread is actually ok, as long as this thread does not access
          // the channel anymore.
          println("delegation of session channel: " + tree)
          sessionEnvironment = sessionEnvironment.delegation(fun.symbol, getSessionChannels(args))
          super.traverse(tree)

        case Assign(lhs,rhs) if isSessionChannelIdent(rhs) => linearityError(lhs,rhs)
        case ValDef(_,name,_,rhs) if isSessionChannelIdent(rhs) => linearityError(name,rhs)

        case _ =>
          super.traverse(tree)
      }
    }
  }

  def newPhase(_prev: Phase) = new StdPhase(_prev) {
    def apply(unit: global.CompilationUnit): Unit = {
      println("AcceptBlockPass starting")
      val unitPath = new File(unit.source.path).getParent()
      val typeChecker = new SessionTypecheckingTraverser(unitPath)
      typeChecker(unit.body)
    }
  }
}