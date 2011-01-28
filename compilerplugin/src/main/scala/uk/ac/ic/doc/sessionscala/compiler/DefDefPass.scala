package uk.ac.ic.doc.sessionscala.compiler

import tools.nsc.plugins.PluginComponent
import tools.nsc.{Global, Phase}

/**
 * Created by: omp08
 */

abstract class DefDefPass extends PluginComponent
                             with SessionTypingEnvironments
                             with SessionDefinitions
                             with SessionTypeCheckingTraversers {
  import global._

	class SessionMethodDefTraverser extends SessionTypeCheckingTraverser {
    def initEnvironment = new MethodSessionTypeInferenceTopLevelEnv

    override def visitSessionMethod(method: Symbol, body: Tree, chanNames: List[Name]) {
      println("visit session method, chans: " + chanNames)
      val savedEnv = env
      env = env.enterSessionMethod(method, chanNames)
      // needs to be here because it requires env to know about the channels in chanNames
      val retChanNames = checkReturnedChannels(body, savedEnv)
      println("&&&&& retChanNames: " + retChanNames)
      traverse(body)
      env = env.leaveSessionMethod(retChanNames) // chanNames here is hack to get current tests to pass again
    }

    def checkReturnedChannels(body: Tree, savedEnv: SessionTypingEnvironment): List[Name] = {
      var last: List[Name] = null
      getFinalExprs(body) foreach { expr =>
        val chans = getSessionChannels(expr)
        if (last == null) last = chans
        else if (chans == last) last = chans
        else throw new RecoverableTypeCheckingException(
          "Inconsistent return of session channels: " + last + " and " + chans, savedEnv)
      }
      assert(last != null) // there should always be an expression
      last
    }

    def isTupleTpt(tpt: Tree) = definitions.isTupleType(tpt.symbol.tpe)

    def getSessionChannels(tuple: Tree): List[Name] = tuple match {
      case Apply(Select(New(tpt),nme.CONSTRUCTOR),args) if isTupleTpt(tpt) =>
        getSessionChannels(args)
      case SessionChannel(name) => List(name)
      case _ => Nil
    }

    def getFinalExprs(trees: List[Tree]): List[Tree] = trees.map(getFinalExprs(_)).flatten
    def getFinalExprs(tree: Tree): List[Tree] = tree match {
      case Block(stats, expr) =>
        getFinalExprs(expr)
      case CaseDef(pat, guard, body) =>
        getFinalExprs(body)
      case If(cond, thenp, elsep) =>
        List(getFinalExprs(thenp), getFinalExprs(elsep)).flatten
      case Match(selector, cases) =>
        getFinalExprs(cases)
      case Return(expr) =>
        // fixme: not sure what to do here. As it is (Nil), unsafe. Maybe forbid returns of session chans altogether
        Nil
      case Try(block, catches, finalizer) =>
        List(getFinalExprs(block), getFinalExprs(catches), getFinalExprs(finalizer)).flatten
      case Typed(expr, tpt) =>
        getFinalExprs(expr)
      case TypeApply(fun, args) =>
        getFinalExprs(fun)
      case Apply(_, _) | Super(_, _) | Function(_, _)
         | Assign(_, _) | New(_) | Throw(_) | This(_)
         | Select(_, _) | Ident(_) | Literal(_) =>
        List(tree)
      case x => throw new IllegalArgumentException("not an expression: " + x)
    }
  }

  def newPhase(_prev: Phase) = new StdPhase(_prev) {
    def apply(unit: global.CompilationUnit): Unit = {
      println("DefDefPass starting")

      //global.treeBrowsers.create().browse(unit.body)
      val inferenceTraverser = new SessionMethodDefTraverser
      inferenceTraverser(unit.body)
      println(inferenceTraverser.env)
      nextPass.inferred = inferenceTraverser.env.inferred.asInstanceOf[nextPass.InferredTypeRegistry]
    }
  }

  val nextPass: JoinBlocksPass
}