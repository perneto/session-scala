package uk.ac.ic.doc.sessionscala.compiler

import org.scribble.protocol.parser.antlr.ANTLRProtocolParser
import org.scribble.protocol.model.ProtocolModel
import tools.nsc.plugins.PluginComponent
import util.control.ControlThrowable
import java.io.{File, FileInputStream}
import tools.nsc.Phase


abstract class JoinBlocksPass extends PluginComponent
                                 with SessionTypingEnvironments
                                 with ScribbleModelFactories
                                 with SessionTypedElementsComponent
                                 with SessionDefinitions
                                 with SessionTypeCheckingTraversers
                                 with ScalaTypeSystemComponent {
  import global._

  var inferred: InferredTypeRegistry = null

  class AbortException extends ControlThrowable

  class JoinBlocksTraverser(unitPath: String) extends SessionTypeCheckingTraverser {

    val scribbleParser = new ANTLRProtocolParser
    def initEnvironment = new JoinBlockTopLevelEnv(inferred)

    def parseFile(filename: String, pos: Position): ProtocolModel = {
      var globalModel: ProtocolModel = null;
      val is = new FileInputStream(new File(unitPath, filename))
      globalModel = scribbleParser.parse(is, scribbleJournal)
      //println("global model: " + globalModel)
      //todo: validate model
      if (globalModel == null) {
        reporter.error(pos,
          "Could not parse scribble description at: " + filename)
        throw new AbortException
      }
      globalModel
    }

    override def visitSessionMethod(method: Symbol, body: Tree, chanNames: List[Name]) {
      // do nothing, and skip visiting the method body as it's checked by DefDefPass
    }

    override def traverse(tree: Tree) {
      val sym = tree.symbol

      tree match {
        case ValDef(_,name,_,rhs)
        if sym.hasAnnotation(protocolAnnotation) && sym.tpe <:< sharedChannelTrait.tpe =>
          val annotInfo = sym.getAnnotation(protocolAnnotation).get
          val (filenameTree: Literal)::_ = annotInfo.args
          val filename = filenameTree.value.stringValue
          val globalModel = parseFile(filename, tree.pos)
          env = env.registerSharedChannel(name, globalModel)
          traverse(rhs)

        case ValDef(_,_,_,_) if sym.hasAnnotation(protocolAnnotation) =>
          reporter.warning(tree.pos, "The @protocol annotation only has an effect on SharedChannel instances")
          super.traverse(tree)

        case Apply(Apply(Select(Ident(chanIdent), _), Apply(_, Literal(role)::Nil)::Nil),
                   Function(ValDef(_,sessChan,_,_)::Nil, block)::Nil)
        if sym == joinMethod =>
          //println("join: " + role + ", sessChan: " + sessChan)
          try {
            pos = tree.pos
            env = env.enterJoin(chanIdent, role.stringValue, sessChan)
            traverse(block)
            pos = tree.pos
            env = env.leaveJoin
          } catch {
            case e: SessionTypeCheckingException =>
              reporter.error(pos, e.getMessage)
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