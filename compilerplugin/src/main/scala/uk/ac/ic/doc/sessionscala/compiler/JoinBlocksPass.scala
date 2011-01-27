package uk.ac.ic.doc.sessionscala.compiler

import org.scribble.protocol.parser.antlr.ANTLRProtocolParser
import org.scribble.protocol.model.ProtocolModel
import tools.nsc.plugins.PluginComponent
import scala.util.control.ControlThrowable
import java.io._
import tools.nsc.Phase


abstract class JoinBlocksPass extends PluginComponent
                                 with SessionTypingEnvironments
                                 with SessionDefinitions
                                 with SessionTypeCheckingTraversers {
  import global._

  var inferred: InferredTypeRegistry = null

  class AbortException extends ControlThrowable

  class JoinBlocksTraverser(unitPath: String) extends SessionTypeCheckingTraverser {

    val scribbleParser = new ANTLRProtocolParser
    def initEnvironment = new JoinBlocksPassTopLevelEnv(inferred)

    def parseFile(filename: String, pos: Position) = {
      val is = new FileInputStream(new File(unitPath, filename))
      parseStream(is, pos, " in file " + filename)
    }

    def parseString(proto: String, pos: Position) = {
      val is = new ByteArrayInputStream(proto.getBytes()) // todo: find out which charset the Scribble parser supports
      parseStream(is, pos, proto)
    }

    def parseStream(is: InputStream, pos: Position, location: String) = {
      var globalModel: ProtocolModel = null;
      globalModel = scribbleParser.parse(is, scribbleJournal)
      //println("global model: " + globalModel)
      //todo: validate model
      if (globalModel == null) {
        reporter.error(pos, "Could not parse Scribble protocol " + location)
        throw new AbortException
      }
      globalModel
    }

    override def visitSessionMethod(method: Symbol, body: Tree, chanNames: List[Name]) {
      // do nothing, and skip visiting the method body as it's checked by DefDefPass
    }

    def hasProtocolAnnotation(sym: Symbol) =
      sym.hasAnnotation(protocolAnnotation) || sym.hasAnnotation(inlineProtocolAnnotation)


    def getStringLiteralArg(sym: Symbol, annotation: Symbol) = {
      val (literal: Literal)::_ = sym.getAnnotation(annotation).get.args
      literal.value.stringValue
    }

    def parseProtocol(sym: Symbol, pos: Position) = {
      if (sym.hasAnnotation(protocolAnnotation)) {
        val filename = getStringLiteralArg(sym, protocolAnnotation)
        parseFile(filename, pos)
      } else {
        val protocol = getStringLiteralArg(sym, inlineProtocolAnnotation)
        parseString(protocol, pos)
      }
    }

    override def traverse(tree: Tree) {
      val sym = tree.symbol
      pos = tree.pos

      tree match {
        case ValDef(_,name,_,rhs)
        if hasProtocolAnnotation(sym) && sym.tpe <:< sharedChannelTrait.tpe =>
          val globalModel = parseProtocol(sym, tree.pos)
          env = env.registerSharedChannel(name, globalModel)
          traverse(rhs)

        case ValDef(_,_,_,_) if hasProtocolAnnotation(sym) =>
          reporter.warning(tree.pos, "The @protocol and @inlineprotocol annotations only have an effect on SharedChannel instances")
          super.traverse(tree)

        case Apply(Apply(Select(Ident(chanIdent), _), Apply(_, Literal(role)::Nil)::Nil),
                   Function(ValDef(_,sessChan,_,_)::Nil, block)::Nil)
        if sym == joinMethod =>
          //println("join: " + role + ", sessChan: " + sessChan)
          try {
            env = env.enterJoin(chanIdent, role.stringValue, sessChan)
            traverse(block)
            pos = tree.pos
            println("leaveJoin")
            env = env.leaveJoin
          } catch {
            case rex: RecoverableTypeCheckingException =>
              reporter.error(pos, rex.getMessage)
              env = rex.recoveryEnv
            case e: SessionTypeCheckingException =>
              reporter.error(pos, e.getMessage)
              //e.printStackTrace()
          }

        case _ =>
          super.traverse(tree)
      }
    }
  }

  def newPhase(_prev: Phase) = new StdPhase(_prev) {
    def apply(unit: global.CompilationUnit): Unit = {
      println("JoinBlockPass starting")
      val unitPath = new File(unit.source.path).getParent()
      val typeChecker = new JoinBlocksTraverser(unitPath)
      try {
        typeChecker(unit.body)
      } catch {
        case e: AbortException =>
      }
    }
  }
}