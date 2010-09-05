package uk.ac.ic.doc.sessionscala.compiler

import org.scribble.protocol.parser.antlr.ANTLRProtocolParser
import org.scribble.protocol.model.ProtocolModel
import tools.nsc.plugins.PluginComponent
import tools.nsc.Phase
import java.io.{File, FileInputStream}

abstract class JoinBlocksPass extends PluginComponent
                                  with SessionTypingEnvironments
                                  with SessionDefinitions {
  import global._

  class AbortException extends Exception

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
      def parseFile(filename: String): ProtocolModel = {
        var globalModel: ProtocolModel = null;
        val is = new FileInputStream(new File(unitPath, filename))
        globalModel = scribbleParser.parse(is, scribbleJournal)
        println("global model: " + globalModel)
        //todo: validate model
        if (globalModel == null) {
          reporter.error(tree.pos,
            "Could not parse scribble description at: " + filename)
          throw new AbortException
        }
        globalModel
      }

      tree match {
        case ValDef(_,name,_,rhs)
        if sym.hasAnnotation(protocolAnnotation) && sym.tpe <:< sharedChannelTrait.tpe =>
          val annotInfo = sym.getAnnotation(protocolAnnotation).get
          val (filenameTree: Literal)::_ = annotInfo.args
          val filename = filenameTree.value.stringValue
          val globalModel = parseFile(filename)
          sessionEnvironment = sessionEnvironment.registerSharedChannel(name, globalModel)
          traverse(rhs)

        case ValDef(_,_,_,_) if sym.hasAnnotation(protocolAnnotation) =>
          reporter.warning(tree.pos, "The @protocol annotation only has an effect on SharedChannel instances")
          super.traverse(tree)

        case Apply(Apply(Select(Ident(chanIdent), _), Literal(role)::Nil),
                   Function(ValDef(_,sessChan,_,_)::Nil, block)::Nil)
        if sym == acceptMethod =>
          println("join: " + role + ", sessChan: " + sessChan)
          sessionEnvironment = sessionEnvironment.enterJoin(chanIdent, role.stringValue, sessChan)
          traverse(block)
          sessionEnvironment = sessionEnvironment.leaveJoin

        case Apply(Select(Apply(Select(Ident(session),_),Literal(role)::Nil),_),arg::Nil)
        if sym == bangMethod && sessionEnvironment.isSessionChannel(session) =>
          println("bangMethod, arg: " + arg + ", arg.tpe: " + arg.tpe + ", session: " + session + ", role: " + role)
          sessionEnvironment = sessionEnvironment.send(session, role.stringValue, arg.tpe)
          traverse(arg)

        case TypeApply(
               Select(
                 Apply(
                   Select(Ident(session), _),
                   Literal(role)::Nil
                 ),
               _),
             _)
        if sym == qmarkMethod && sessionEnvironment.isSessionChannel(session) =>
          if (tree.tpe == definitions.getClass("scala.Nothing").tpe)
            reporter.error(tree.pos, "Method ? needs to be annotated with explicit type")
          println("qmarkMethod, tree.tpe:" + tree.tpe + ", session: " + session + ", role: " + role)
          sessionEnvironment = sessionEnvironment.receive(session, role.stringValue, tree.tpe)
          super.traverse(tree)

        case Apply(
               TypeApply(
                 Select(
                   Apply(
                     Select(Ident(session),_),
                     Literal(role)::Nil
                   ), _
                 ), _
               ),
               Function(_,Match(_,cases))::Nil
             )
        if sym == receiveMethod && sessionEnvironment.isSessionChannel(session) =>
            println("receiveMethod, session: " + session + ", role: " + role
                    + ", cases: " + cases)
            sessionEnvironment = sessionEnvironment.enterBranchReceiveBlock(session, role.stringValue)
            cases foreach { c: CaseDef =>
              if (! c.guard.isEmpty) {
                reporter.error(c.guard.pos, "Receive clauses on session channels (branching) do not support guards yet")
              } else {
                def processBranch = {
                  sessionEnvironment = sessionEnvironment.enterIndividualBranchReceive(c.pat.tpe)
                  traverse(c.body)
                  sessionEnvironment = sessionEnvironment.leaveIndividualBranchReceive
                }
                c.pat match {
                  case Select(_,name) => processBranch
                  case Ident(name) => processBranch
                  case _ =>
                    reporter.error(c.pat.pos, "Receive clauses on session channels (branching) do not support complex patterns yet")
                }

              }
            }
            sessionEnvironment = sessionEnvironment.leaveBranchReceiveBlock

        case Apply(fun,args) if !getSessionChannels(args).isEmpty =>
          println("delegation of session channel: " + tree)
          sessionEnvironment = sessionEnvironment.delegation(fun.symbol, getSessionChannels(args))
          super.traverse(tree)

        // todo: allow returning session channel from methods after they have advanced the session
        // need to be assigned to new val, new val identifier need to be added to environment
        //case ValDef(_,name,_,a @ Apply(fun,_)) if a.symbol.tpe == sessionChannelType =>

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
      try {
        typeChecker(unit.body)
      } catch {
        case e: AbortException =>
      }
    }
  }
}