package uk.ac.ic.doc.sessionscala.compiler


import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

/*
class SessionTypingPlugin(val global: Global) extends Plugin {
  import global._

  val name = "sessiontyping"
  val description = "Typechecks actors using session-scala library"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent {
    val global = SessionTypingPlugin.this.global
    val runsAfter = List[String]("refchecks");
    val phaseName = name
    def newPhase(_prev: Phase) = new SessionTypingPhase(_prev)

    class SessionTypingPhase(prev: Phase) extends StdPhase(prev) {
      override def name = SessionTypingPlugin.this.name
      def apply(unit: CompilationUnit) {
        for ( tree @ Apply(Select(rcvr, nme.DIV), List(Literal(Constant(0)))) <- unit.body;
             if rcvr.tpe <:< definitions.IntClass.tpe)
          {
            unit.warning(tree.pos, "Calling accept")
          }
      }
    }
  }

}
*/