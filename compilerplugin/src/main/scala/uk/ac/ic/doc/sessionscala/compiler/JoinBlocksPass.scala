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
    def initEnvironment = new ProcessBlocksPassTopLevelEnv(inferred)

    def file(filename: String) = new File(unitPath, filename)
    def parseFile(filename: String, pos: Position) = {
      val is = new FileInputStream(file(filename))
      parseStream(is, pos, " in file " + filename)
    }

    def parseString(proto: String, pos: Position) = {
      val is = new ByteArrayInputStream(proto.getBytes()) // todo: find out which charset the Scribble parser supports
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

    lazy val stringType = definitions.StringClass.tpe
    lazy val function1 = definitions.FunctionClass(1)
    def isSharedChannelFunction(tpe: Type): Boolean = tpe match {
      case TypeRef(_, function1, List(paramTpe,_)) if paramTpe <:< sharedChannelTrait.tpe =>
        true
      case x =>
        println("Not shared channel function: " + x)
        false
    }
    def takesSharedChannel(tpe: Type): Boolean = tpe match {
      case PolyType(
        _typeParam,
        MethodType(
          stringType::_,
          MethodType(
            functionParamSymbol::Nil,
            _)))
      if isSharedChannelFunction(functionParamSymbol.tpe) =>
        //println("FOUND SHARED CHANNEL: " + tpe + ", functionParamSymbol: " + functionParamSymbol)
        true
      case x =>
        //println("not shared channel function: " + tpe)
        false
    }

    override def visitSessionMethod(method: Symbol, body: Tree, chanNames: List[Name]) {
      env = env.enterSessionMethod(method, chanNames)
      //println("@@@@@@@@@@@@ traversing method body: " + method)
      traverse(body)
      env = env.leaveSessionMethod(Nil)
    }

    def getStringLiteralArg(sym: Symbol, annotation: Symbol) = {
      val (literal: Literal)::_ = sym.getAnnotation(annotation).get.args
      literal.value.stringValue
    }

    def parseProtocol(args: List[Tree], pos: Position) = args match {
      case Literal(protocol)::_ =>
        val proto = protocol.stringValue
        if (file(proto).canRead())
          parseFile(proto, pos)
        else
          parseString(proto, pos)
    }

    def getRoles(args: List[Tree]): List[String] = {
      val result = args map {
        case Apply(
          TypeApply(
            Select(
              Apply(_,SymbolMatcher(role)::Nil),
              _minusgt)
            ,_)
          ,_) => role
      }
      //println("getRoles: " + result)
      result
    }

    def visitSharedChanCreation(pos: Position, chanName: Name, body: Tree, args: List[Tree]) {
      //println("shared channel creation: " + sym + ", channel name: " + name)
      val globalModel = parseProtocol(args, pos)
      env = env.registerSharedChannel(chanName, globalModel)
      traverse(body)
    }

    var sharedChanBody: Tree = null
    var sharedChanName: Name = null
    var syntheticValName: Name = null
    override def traverse(tree: Tree) {
      val sym = tree.symbol
      pos = tree.pos

      tree match {
        case ValDef(_,syntheticName,_, f@Function(ValDef(_,name,_,_)::Nil, body))
        if isSharedChannelFunction(f.tpe) =>
          //println("FOUND synthetic var: " + syntheticName)
          sharedChanName = name
          sharedChanBody = body
          syntheticValName = syntheticName

        case Apply(Apply(_,args), Function(ValDef(_,name,_,_)::Nil, body)::Nil)
        if takesSharedChannel(sym.tpe) =>
          visitSharedChanCreation(tree.pos, name, body, args)

        case Apply(Apply(_,args), Ident(name)::Nil) if syntheticValName != null && name == syntheticValName =>
          val body = sharedChanBody
          sharedChanBody = null // can have nested shared channel creations
          val chanName = sharedChanName
          sharedChanName = null
          syntheticValName = null
          visitSharedChanCreation(tree.pos, chanName, body, args)

        case Apply(Apply(Select(Ident(chanIdent), _), Apply(_, Literal(role)::Nil)::Nil),
                   Function(ValDef(_,sessChan,_,_)::Nil, block)::Nil)
        if sym == joinMethod || sym == acceptMethod =>
          //println("join: " + role + ", sessChan: " + sessChan)
          try {
            env = env.enterJoin(chanIdent, role.stringValue, sessChan)
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

        case Apply(Select(Ident(sharedChan),_), args) if sym == inviteMethod =>
          env = env.invite(sharedChan, getRoles(args))

        case _ =>
          super.traverse(tree)
      }
    }
  }

  def newPhase(_prev: Phase) = new StdPhase(_prev) {
    def apply(unit: global.CompilationUnit): Unit = {
      println("   JoinBlockPass starting")
      val unitPath = new File(unit.source.path).getParent()
      val typeChecker = new JoinBlocksTraverser(unitPath)
      try {
        typeChecker(unit.body)
      } catch {
        case _: SessionTypeCheckingException | _: AbortException =>
      }
    }
  }
}