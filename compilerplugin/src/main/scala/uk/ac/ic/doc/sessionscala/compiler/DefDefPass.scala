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
      env = env.enterSessionMethod(method, chanNames)
      traverse(body)
      // todo: go down all paths, record order of returned session channels
      // check that order is consistent across all paths
      env = env.leaveSessionMethod(List())
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