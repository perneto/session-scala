package uk.ac.ic.doc.sessionscala.compiler

import tools.nsc.ast.Trees
import tools.nsc.symtab.Symbols
import tools.nsc.Global

/**
 * Created by: omp08
 */

trait SessionTypeCheckingTraversers { 
  self: SessionDefinitions with SessionTypingEnvironments with ScalaTypeSystemComponent =>
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

    def getType(arg: Tree): Option[Type] = {
      //fixme: nasty hack necessary to get correct type both for class instances and objects (modules)
      val t = definitions.getClass(arg.tpe.typeSymbol.fullName).tpe
      println("t: " + t)
      Some(t)
    }

    def stringValue(const: Constant) = Some(const.stringValue)

    def getLabelAndArgType(arg: Tree): (Option[String], Option[Type]) = {
      println("arg.symbol.tpe: " + (if (arg.symbol == null) "(symbol null)" else arg.symbol.tpe))
      val ret = arg match {
        // The first pattern might be too broad, as it doesn't check that the inner Apply
        // is for a Tuple constructor
        case Apply(_, List(app@Apply(_,Literal(label)::Nil), arg)) if app.symbol == symbolApplyMethod =>
          (stringValue(label), getType(arg))
        case Apply(_,Literal(label)::Nil) if arg.symbol == symbolApplyMethod =>
          (stringValue(label), None)
        case arg =>
          //println("###########  " + arg)
          (None, getType(arg))
      }
      println("getLabelAndArgType, returning: " + ret)
      assert(ret._1.isDefined || ret._2.get != definitions.getClass("scala.Symbol").tpe)
      ret
    }

    override def traverse(tree: Tree) {
      val sym = tree.symbol

      try {
        pos = tree.pos
        tree match {
          // message send. Without label: s('Alice) ! 42
          // with label: s('Alice) ! ('mylabel, 42)
          // or: s('Alice) ! 'quit
          case Apply(Select(Apply(Select(Ident(session),_),Apply(_, Literal(role)::Nil)::Nil),_), arg::Nil)
          if sym == bangMethod && env.isSessionChannel(session) =>
            println("!!!!!!! bangMethod, arg: " + arg + ", arg.tpe: " + arg.tpe
                    + ", session: " + session + ", role: " + role)
            val (lbl,tpe) = getLabelAndArgType(arg)
            env = env.send(session, role.stringValue, MsgSig(lbl, tpe))
            traverse(arg)

          // message receive, no label: s('Alice).?[Int]
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
            println("????? qmarkMethod, tree.tpe:" + tree.tpe + ", session: " + session + ", role: " + role)
            pos = qmark.pos
            env = env.receive(session, role.stringValue, sig(tree.tpe)) // fixme: does this require the type hack as well?
            super.traverse(tree)

          // message receive, with label: val ('quote, quoteVal: Int) = s('Seller).??
          // at the time of plugin execution (after refchecks), transformed into:
          // val quoteVal: Int = (s.apply(scala.Symbol.apply("Seller")).??: Any @unchecked) match {
          //   case (scala.Symbol.unapply("quote"), (quoteVal @ (_: Int))) => quoteVal
          // }
          case ValDef(_, valName, tpt, Match(
            Typed(
              qqMark@Apply(
                Select(
                  Apply(
                    Select(Ident(session), _),
                    Apply(_, Literal(role)::Nil)::Nil
                  ),
                  _
                ),
                Nil
              ),
              _
            ),
            CaseDef(
              app@Apply(
                _,
                UnApply(
                  _,
                  Literal(label)::Nil)::_
                ),
              _,
              _)::Nil
            )) if qqMark.symbol == qqMarkMethod && env.isSessionChannel(session) =>
            println("?????????? message receive, session: " + session + ", role: "
                    + role+", msg type: " + tpt.tpe + ", label: " + label)
            pos = app.pos
            val unitSymbol = definitions.getClass("scala.Unit")
            val tpe = if (tpt.tpe == unitSymbol.tpe) None
            else Some(tpt.tpe)
            env = env.receive(session, role.stringValue, MsgSig(Some(label.stringValue), tpe))

          // receive/react
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
          if (sym == receiveMethod || sym == reactMethod) && env.isSessionChannel(session) =>
              //println("receiveMethod, session: " + session + ", role: " + role
              //        + ", cases: " + cases)
              pos = tree.pos
              env = env.enterChoiceReceiveBlock(session, role.stringValue)
              cases foreach { c: CaseDef =>
                if (! c.guard.isEmpty) {
                  reporter.error(c.guard.pos,
                    "Receive clauses on session channels (branching) do not support guards yet")
                } else {
                  c.pat match {
                    case Select(_,_) | Ident(_) | Bind(_,_) =>
                      val t = c.pat.tpe

                      // fixme: other nasty hack necessary to get correct type both for class instances and objects (modules)
                      val msgTpe = definitions.getClass(t.typeSymbol.fullName).tpe

                      visitBranch(c, sig(msgTpe))
                    case UnApply(fun, Literal(label)::Nil) if fun.symbol == symbolUnapplyMethod =>
                      visitBranch(c, sig(label.stringValue))
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
            env = env.delegation(fun.symbol, getSessionChannels(args), List())
            super.traverse(tree)

          // todo: allow returning session channel from methods after they have advanced the session
          // need to be assigned to new val, new val identifier needs to be added to environment
          //case ValDef(_,name,_,a @ Apply(fun,_)) if a.symbol.tpe == sessionChannelType =>

          // todo: support pattern matching on standard receives, checking that all
          // cases are subtypes of protocol-defined type. (Maybe: enforce complete match?)

          // todo: deal with method nesting and name shadowing

          case Assign(lhs,rhs) if isSessionChannel(rhs) => linearityError(lhs,rhs)
          case ValDef(_,name,_,rhs) if isSessionChannel(rhs) => linearityError(name,rhs)

          case If(cond,thenp,elsep) =>
            pos = thenp.pos
            println("enterThen")
            env = env.enterThen
            println("after enterThen, traversing body")
            traverse(thenp)
            pos = elsep.pos
            println("enterElse")
            env = env.enterElse
            println("after enterElse, traversing body")
            traverse(elsep)
            println("leaveIf")
            env = env.leaveIf
            println("after leaveIf")

          case DefDef(_,name,tparams,_,_,rhs) =>
            //println("method def: " + name + ", symbol: " + tree.symbol)

            val chanNames = sessionChannelNames(tree.symbol.tpe)
            if (!chanNames.isEmpty) {
              if (!tparams.isEmpty) reporter.error(tree.pos,
                  "Type parameters not supported for session methods")
              println("enter session method, chans: " + chanNames)
              visitSessionMethod(tree.symbol, rhs, chanNames)
            } else {
              super.traverse(tree)
            }

          case _ =>
            super.traverse(tree)
        }

      } catch {
        case e: SessionTypeCheckingException =>
          reporter.error(pos, e.getMessage)
          //e.printStackTrace()
      }
    }

    def visitBranch(c: CaseDef, msgSig: MsgSig) {
      pos = c.pat.pos
      env = env.enterChoiceReceiveBranch(msgSig)
      pos = c.body.pos
      traverse(c.body)
      pos = c.body.pos
      env = env.leaveChoiceReceiveBranch
    }

    def visitSessionMethod(method: Symbol, body: Tree, chanNames: List[Name])

    def isSessionChannelType(t: Type): Boolean = {
      val function1 = definitions.FunctionClass(1)
      val sessionChannel = typeRef(function1.owner.tpe, function1,
              List(definitions.SymbolClass.tpe, participantChannelClass.tpe))
      //println(t + " <:< " + sessionChannel + ": " + (t <:< sessionChannel))
      t <:< sessionChannel
    }

    def sessionChannelNames(tpe: Type): List[Name] = tpe match {
      case MethodType(argTypes, _) =>
        (for (argS <- argTypes if isSessionChannelType(argS.tpe))
          yield Some(argS.name)) flatten
      case _ => Nil
    }
    
  }
}