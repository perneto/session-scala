package uk.ac.ic.doc.sessionscala.compiler

import org.scribble.protocol.parser.antlr.ANTLRProtocolParser
import org.scribble.protocol.model.ProtocolModel
import tools.nsc.plugins.PluginComponent
import scala.util.control.ControlThrowable
import java.io._
import tools.nsc.Phase


abstract class BindBlocksPass extends PluginComponent
                                 with SessionTypingEnvironments
                                 with SessionDefinitions
                                 with SessionTypeCheckingTraversers {
  import global._

  var inferred: InferredTypeRegistry = null

  class AbortException extends ControlThrowable

  class BindBlocksTraverser(unitPath: String) extends SessionTypeCheckingTraverser {

    val scribbleParser = new ANTLRProtocolParser
    def initEnvironment = new ProcessBlocksPassTopLevelEnv(inferred)

    def file(filename: String) = new File(unitPath, filename)
    def parseFile(filename: String, pos: Position) = {
      val is = new FileInputStream(file(filename))
      parseStream(is, pos, " in file " + filename)
    }

    def parseString(proto: String, pos: Position) = {
      // Scribble doesn't use any chars outside ascii, so any charset is fine
      val is = new ByteArrayInputStream(proto.getBytes)
      parseStream(is, pos, proto)
    }

    def parseStream(is: InputStream, pos: Position, location: String) = {
      var globalModel: ProtocolModel = null;
      globalModel = scribbleParser.parse(is, scribbleJournal, null)
      //println("global model: " + globalModel)
      //todo: validate model
      if (globalModel == null) {
        reporter.error(pos, "Could not parse Scribble protocol " + location)
        throw new AbortException
      }
      globalModel
    }

    override def visitSessionMethod(method: Symbol, body: Tree, chanNames: List[Name]) {
      env = env.enterSessionMethod(method, chanNames)
      //println("@@@@@@@@@@@@ traversing method body: " + method)
      traverse(body)
      env = env.leaveSessionMethod(Nil)
    }

    def getAddresses(args: List[Tree]): List[Name] = args map {
      case Ident(name) => name
    }

    def parseProtocol(proto: Tree, pos: Position): ProtocolModel = proto match {
      case Literal(protocol) =>
        val proto = protocol.stringValue
        if (file(proto).canRead)
          parseFile(proto, pos)
        else
          parseString(proto, pos)
      case Ident(name) =>
        parseProtocol(strIdents(name), pos)
    }
     
    def visitSharedChanCreation(pos: Position, chanName: Name, proto: Tree, roleName: String) {
      //println("shared channel creation: " + sym + ", channel name: " + name)
      val globalModel = parseProtocol(proto, pos)
      env = env.registerAddress(chanName, globalModel, roleName)
    }

    var strIdents = Map[Name, Literal]()
    override def traverse(tree: Tree) {
      val sym = tree.symbol
      pos = tree.pos

      tree match {
        case ValDef(_,name,_,l:Literal) if sym.tpe <:< definitions.StringClass.tpe =>
          strIdents += name -> l
          
        case ValDef(_,name,_,Apply(_,proto::SymbolMatcher(roleName)::_)) 
        if sym.tpe <:< addressTrait.tpe =>  
          visitSharedChanCreation(pos, name, proto, roleName)
          
        case Apply(TypeApply(SelectIdent(chanIdent),_), Function1(sessChan, block)::Nil)
        if sym == bindMethod =>
          //println("bind: " + chanIdent + ", sessChan: " + sessChan)
          try {
            env = env.enterJoin(chanIdent, sessChan)
            traverse(block)
            pos = tree.pos
            //println("leaveJoin, env: " + env)
            env = env.leaveJoin
          } catch {
            case rex: RecoverableTypeCheckingException =>
              reporter.error(pos, rex.getMessage)
              env = rex.recoveryEnv
            case e: SessionTypeCheckingException =>
              reporter.error(pos, e.getMessage)
              //e.printStackTrace()
          }

        case Apply(_, args) if sym == startSessionMethod =>
          //println("startSession: "+args)
          env = env.invite(getAddresses(args))

        case _ =>
          super.traverse(tree)
      }
    }
  }

  def newPhase(_prev: Phase) = new StdPhase(_prev) {
    def apply(unit: global.CompilationUnit): Unit = {
      //println("   JoinBlockPass starting")
      val unitPath = new File(unit.source.path).getParent
      val typeChecker = new BindBlocksTraverser(unitPath)
      try {
        typeChecker(unit.body)
      } catch {
        case _: SessionTypeCheckingException | _: AbortException =>
      }
    }
  }
}