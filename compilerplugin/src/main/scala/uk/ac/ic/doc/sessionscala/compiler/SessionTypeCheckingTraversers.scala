package uk.ac.ic.doc.sessionscala.compiler

import tools.nsc.ast.Trees
import tools.nsc.symtab.Symbols
import tools.nsc.util.Position
import tools.nsc.Global

/**
 * Created by: omp08
 */

trait SessionTypeCheckingTraversers { self: SessionDefinitions =>
  val global: Global
  import global._

  trait SessionTypeCheckingTraverser extends Traverser {

    def send(sessionChan: Name, dstRole: String, tpe: Type): Unit
    def receive(sessionChan: Name, srcRole: String, tpe: Type): Unit
    def enterChoiceReceiveBlock(sessionChan: Name, srcRole: String): Unit
    def enterChoiceReceiveBranch(label: Type): Unit
    def leaveChoiceReceiveBranch: Unit
    def leaveChoiceReceiveBlock: Unit
    def enterThen: Unit
    def enterElse: Unit
    def leaveIf: Unit
    def delegation(fun: Symbol, sessChans: List[Name]): Unit
    def isSessionChannel(name: Name): Boolean

    def linearityError(lhs: Any, rhs: Tree) {
      reporter.error(rhs.pos, "Cannot assign " + rhs
        + " to " + lhs + ": aliasing of session channels is forbidden")
    }
    def isSessionChannel(tree: Tree): Boolean = getSessionChannelName(tree).isDefined

        def getSessionChannelName(tree: Tree): Option[Name] = tree match {
          case Ident(name) if isSessionChannel(name) => Some(name)
          case _ => None
        }

        def getSessionChannels(args: List[Tree]): List[Name] =
          args.map(getSessionChannelName).flatten
    
    override def traverse(tree: Tree) {
      val sym = tree.symbol

      tree match {
        case Apply(Select(Apply(Select(Ident(session),_),Literal(role)::Nil),_),arg::Nil)
        if sym == bangMethod && isSessionChannel(session) =>
          //println("bangMethod, arg: " + arg + ", arg.tpe: " + arg.tpe
          //        + ", session: " + session + ", role: " + role)
          send(session, role.stringValue, arg.tpe)
          traverse(arg)

        case TypeApply(
               Select(
                 Apply(
                   Select(Ident(session), _),
                   Literal(role)::Nil
                 ),
               _),
             _)
        if sym == qmarkMethod && isSessionChannel(session) =>
          if (tree.tpe == definitions.getClass("scala.Nothing").tpe)
            reporter.error(tree.pos, "Method ? needs to be annotated with explicit type")
          //println("qmarkMethod, tree.tpe:" + tree.tpe + ", session: " + session + ", role: " + role)
          receive(session, role.stringValue, tree.tpe)
          super.traverse(tree)

        case Apply(
               TypeApply(
                 Select(
                   Apply(
                     Select(Ident(session),_),
                     Literal(role)::Nil
                   ), _
                 ), _
               ),
               Function(_,Match(_,cases))::Nil
             )
        if sym == receiveMethod && isSessionChannel(session) =>
            //println("receiveMethod, session: " + session + ", role: " + role
            //        + ", cases: " + cases)
            enterChoiceReceiveBlock(session, role.stringValue)
            cases foreach { c: CaseDef =>
              if (! c.guard.isEmpty) {
                reporter.error(c.guard.pos,
                  "Receive clauses on session channels (branching) do not support guards yet")
              } else {
                def processBranch = {
                  enterChoiceReceiveBranch(c.pat.tpe)
                  traverse(c.body)
                  leaveChoiceReceiveBranch
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
            leaveChoiceReceiveBlock

        case Apply(fun,args) if !getSessionChannels(args).isEmpty =>
          println("delegation of session channel: " + tree)
          delegation(fun.symbol, getSessionChannels(args))
          super.traverse(tree)

        // todo: allow returning session channel from methods after they have advanced the session
        // need to be assigned to new val, new val identifier needs to be added to environment
        //case ValDef(_,name,_,a @ Apply(fun,_)) if a.symbol.tpe == sessionChannelType =>

				// todo: support pattern matching on standard receives, checking that all
				// cases are subtypes of protocol-defined type. (Maybe: enforce complete match?)

        case Assign(lhs,rhs) if isSessionChannel(rhs) => linearityError(lhs,rhs)
        case ValDef(_,name,_,rhs) if isSessionChannel(rhs) => linearityError(name,rhs)

        case If(cond,thenp,elsep) =>
          enterThen
          traverse(thenp)
          enterElse
          traverse(elsep)
          leaveIf

        case _ =>
          super.traverse(tree)
      }
    }
  }
}