package uk.ac.ic.doc.sessionscala.compiler

import tools.nsc.ast.Trees
import tools.nsc.symtab.Symbols
import tools.nsc.Global

/**
 * Created by: omp08
 */

trait SessionTypeCheckingTraversers { 
  self: SessionDefinitions with SessionTypingEnvironments =>
  val global: Global
  import global._

  trait SessionTypeCheckingTraverser extends Traverser {
    def initEnvironment: SessionTypingEnvironment
    var env = initEnvironment
    var pos: Position = NoPosition
    
    def linearityError(lhs: Any, rhs: Tree) {
      reporter.error(rhs.pos, "Cannot assign " + rhs
        + " to " + lhs + ": aliasing of session channels is forbidden")
    }
    def isSessionChannel(tree: Tree): Boolean = getSessionChannelName(tree).isDefined

    def getSessionChannelName(tree: Tree): Option[Name] = tree match {
      case Ident(name) if env.isSessionChannel(name) => Some(name)
      case _ => None
    }

    def getSessionChannels(args: List[Tree]): List[Name] =
      args.map(getSessionChannelName).flatten
    
    override def traverse(tree: Tree) {
      val sym = tree.symbol

      try {
        tree match {
          case Apply(Select(Apply(Select(Ident(session),_),Apply(_, Literal(role)::Nil)::Nil),_),arg::Nil)
          if sym == bangMethod && env.isSessionChannel(session) =>
            //println("bangMethod, arg: " + arg + ", arg.tpe: " + arg.tpe
            //        + ", session: " + session + ", role: " + role)
            pos = tree.pos
            env = env.send(session, role.stringValue, arg.tpe)
            traverse(arg)

          case TypeApply(
                 qmark@Select(
                   Apply(
                     Select(Ident(session), _),
                     Apply(_, Literal(role)::Nil)::Nil
                   ),
                 _),
               _)
          if sym == qmarkMethod && env.isSessionChannel(session) =>
            if (tree.tpe == definitions.getClass("scala.Nothing").tpe)
              reporter.error(tree.pos, "Method ? needs to be annotated with explicit type")
            //println("qmarkMethod, tree.tpe:" + tree.tpe + ", session: " + session + ", role: " + role)
            pos = qmark.pos
            env = env.receive(session, role.stringValue, tree.tpe)
            super.traverse(tree)

          case Apply(
                 TypeApply(
                   Select(
                     Apply(
                       Select(Ident(session),_),
                       Apply(_, Literal(role)::Nil)::Nil
                     ), _
                   ), _
                 ),
                 (f@Function(_,Match(_,cases)))::Nil
               )
          if sym == receiveMethod && env.isSessionChannel(session) =>
              //println("receiveMethod, session: " + session + ", role: " + role
              //        + ", cases: " + cases)
              pos = tree.pos
              env = env.enterChoiceReceiveBlock(session, role.stringValue)
              cases foreach { c: CaseDef =>
                if (! c.guard.isEmpty) {
                  reporter.error(c.guard.pos,
                    "Receive clauses on session channels (branching) do not support guards yet")
                } else {
                  def processBranch = {
                    pos = c.pat.pos
                    env = env.enterChoiceReceiveBranch(c.pat.tpe)
                    traverse(c.body)
                    pos = c.body.pos
                    env = env.leaveChoiceReceiveBranch
                  }
                  c.pat match {
                    case Select(_,name) => processBranch
                    case Ident(name) => processBranch
                    case _ =>
                      reporter.error(c.pat.pos,
                        "Receive clauses on session channels (branching) do not support complex patterns yet")
                  }

                }
              }
              pos = f.pos
              env = env.leaveChoiceReceiveBlock

          case Apply(fun,args) if !getSessionChannels(args).isEmpty =>
            println("delegation of session channel: " + tree)
            pos = tree.pos
            env = env.delegation(fun.symbol, getSessionChannels(args))
            super.traverse(tree)

          // todo: allow returning session channel from methods after they have advanced the session
          // need to be assigned to new val, new val identifier needs to be added to environment
          //case ValDef(_,name,_,a @ Apply(fun,_)) if a.symbol.tpe == sessionChannelType =>

          // todo: support pattern matching on standard receives, checking that all
          // cases are subtypes of protocol-defined type. (Maybe: enforce complete match?)

          case Assign(lhs,rhs) if isSessionChannel(rhs) => linearityError(lhs,rhs)
          case ValDef(_,name,_,rhs) if isSessionChannel(rhs) => linearityError(name,rhs)

          case If(cond,thenp,elsep) =>
            pos = thenp.pos
            env = env.enterThen
            traverse(thenp)
            pos = elsep.pos
            env = env.enterElse
            traverse(elsep)
            env = env.leaveIf

          case _ =>
            super.traverse(tree)
        }

      } catch {
        case e: SessionTypeCheckingException =>
          reporter.error(pos, e.getMessage)
          throw e
      }
    }
  }
}