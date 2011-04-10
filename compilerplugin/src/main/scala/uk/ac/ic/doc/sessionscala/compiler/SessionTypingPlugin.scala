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

  val bindBlocksPass = new BindBlocksPass {
    val global = SessionTypingPlugin.this.global
    val runsAfter = List[String]("sessiontyping_methoddefs")
    val phaseName = "sessiontyping_acceptblocks"

    val scribbleJournal = _journal
  }

  val methodDefPass = new SessionMethodDefPass {
    val global = SessionTypingPlugin.this.global
    val runsAfter = List[String]("refchecks")
    val phaseName = "sessiontyping_methoddefs"

    val scribbleJournal = _journal
    val nextPass = bindBlocksPass
  }

  val components = List[PluginComponent](bindBlocksPass, methodDefPass)
}