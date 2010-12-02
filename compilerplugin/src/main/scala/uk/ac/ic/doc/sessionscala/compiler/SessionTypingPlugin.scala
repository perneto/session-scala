package uk.ac.ic.doc.sessionscala.compiler

import scala.tools.nsc
import nsc.Global
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import org.scribble.common.logging.ConsoleJournal

class SessionTypingPlugin(val global: Global) extends Plugin {
  val name = "sessiontyping"
  val description = "Multiparty Session Type checking for actors using the session-scala library"
  val _journal = new ConsoleJournal

  val joinBlockPass = new JoinBlocksPass {
    val global = SessionTypingPlugin.this.global
    val runsAfter = List[String]("sessiontyping_methoddefs")
    val phaseName = "sessiontyping_acceptblocks"

    val scribbleJournal = _journal
  }

  val methodDefPass = new DefDefPass {
    val global = SessionTypingPlugin.this.global
    val runsAfter = List[String]("refchecks")
    val phaseName = "sessiontyping_methoddefs"

    val scribbleJournal = _journal
    val nextPass = joinBlockPass
  }

  val components = List[PluginComponent](joinBlockPass, methodDefPass)
}