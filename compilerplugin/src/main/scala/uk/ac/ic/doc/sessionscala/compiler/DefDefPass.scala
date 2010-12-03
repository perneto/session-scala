package uk.ac.ic.doc.sessionscala.compiler

import tools.nsc.plugins.PluginComponent
import tools.nsc.{Global, Phase}

/**
 * Created by: omp08
 */

abstract class DefDefPass extends PluginComponent
                             with SessionTypingEnvironments
                             with ScalaTypeSystemComponent
                             with SessionDefinitions
                             with SessionTypeCheckingTraversers {
  import global._

	class SessionMethodDefTraverser extends SessionTypeCheckingTraverser {
    def initEnvironment = new MethodSessionTypeInferenceTopLevelEnv

    override def visitSessionMethod(method: Symbol, body: Tree, chanNames: List[Name]) {
      env = env.enterSessionMethod(method, chanNames)
      traverse(body)
      env = env.leaveSessionMethod
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