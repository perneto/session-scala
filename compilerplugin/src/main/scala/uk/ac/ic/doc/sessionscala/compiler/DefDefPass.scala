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

	def env: SessionTypingEnvironment = new JoinBlockTopLevelEnv

  def isSessionChannelType(t: Type): Boolean = {
    val function1 = definitions.FunctionClass(1)
    val sessionChannel = typeRef(function1.owner.tpe, function1,
            List(definitions.StringClass.tpe, participantChannelClass.tpe))
    //println(t + " <:< " + sessionChannel)
    t <:< sessionChannel
  }

  def sessionChannelName(tpe: Type): List[Name] = tpe match {
    case MethodType(argTypes, _) =>
      (for (argS <- argTypes if isSessionChannelType(argS.tpe))
        yield Some(argS.name)) flatten
    case _ => Nil
  }
  
  class SessionMethodDefTraverser extends SessionTypeCheckingTraverser {
    def leaveIf = {}

    def enterElse = {}

    def enterThen = {}

    def leaveChoiceReceiveBlock = {}

    def leaveChoiceReceiveBranch = {}


    def isSessionChannel(name: Name) = false

    def delegation(fun: Symbol, sessChans: List[Name]) = null

    def enterChoiceReceiveBranch(label: Type) = null

    def enterChoiceReceiveBlock(sessionChan: Name, srcRole: String) = null

    def receive(sessionChan: Name, srcRole: String, tpe: Type) = null

    def send(sessionChan: Name, dstRole: String, tpe: Type) = null

    override def traverse(tree: Tree) = tree match {
			case DefDef(_,name,tparams,vparamss,tpt,rhs) =>
	    println("method def: " + name + ", symbol: " + tree.symbol)
	
	    val chanNames = sessionChannelName(tree.symbol.tpe)
	    if (!chanNames.isEmpty) {
	      if (!tparams.isEmpty) reporter.error(tree.pos,
	          "Type parameters not supported for session methods")
	      println(chanNames)
	    }
				super.traverse(tree)
			case _ => super.traverse(tree)
		}
  }

  def newPhase(_prev: Phase) = new StdPhase(_prev) {
    def apply(unit: global.CompilationUnit): Unit = {
      println("DefDefPass starting")

      //global.treeBrowsers.create().browse(unit.body)
      val typeChecker = new SessionMethodDefTraverser
      typeChecker(unit.body)
    }
  }

}