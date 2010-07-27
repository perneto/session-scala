package uk.ac.ic.doc.sessionscala.compiler

import tools.nsc.plugins.PluginComponent
import tools.nsc.Phase

/**
 * Created by: omp08
 */

abstract class DefDefPass extends PluginComponent
                                  with SessionTypingEnvironments
                                  with SessionDefinitions {
  import global._

  class SessionMethodDefTraverser extends Traverser {
    override def traverse(tree: Tree) {}
  }

  def newPhase(_prev: Phase) = new StdPhase(_prev) {
    def apply(unit: global.CompilationUnit): Unit = {
      println("DefDefPass starting")

      global.treeBrowsers.create().browse(unit.body)
      val typeChecker = new SessionMethodDefTraverser
      typeChecker(unit.body)
    }
  }

}