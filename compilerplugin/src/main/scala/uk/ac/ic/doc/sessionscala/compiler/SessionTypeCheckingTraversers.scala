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

    import PartialFunction.condOpt
    object SelectIdent {
      def unapply(t: Tree) = condOpt(t) {
        case Select(Ident(name), _) => name
      }
    }
    
    object SessionIdent {
      def unapply(t: Tree) = condOpt(t) {
        case SelectIdent(session) if env.isSessionChannel(session) => session
      }
    }

    object Apply1 {
      def unapply(t: Tree) = condOpt(t) {
        case Apply(f, arg::Nil) => (f,arg)
      }
    }

    object UnApply1 {
      def unapply(t: Tree) = condOpt(t) {
        case UnApply(f, arg::Nil) => (f,arg)
      }
    }

    object ApplyArg {
      def unapply(t: Tree) = condOpt(t) {
        case ApplyArgs(arg::Nil) => arg
      }
    }

    object ApplyArgs {
      def unapply(t: Tree) = condOpt(t) {
        case (Apply(_, args)) => args
      }
    }

    object Function1 {
      def unapply(t: Tree) = condOpt(t) {
        case Function(ValDef(_,name,_,_)::Nil, body) => (name, body)
      }
    }

    object SessionChannel {
      def unapply(t: Tree) = condOpt(t) {
        case Ident(name) if env.isSessionChannel(name) => name
      }
    }

    object Channel {
     def unapply(t: Tree) = condOpt(t) {
       case SessionChannel(name) => name
       case Ident(name) if env.isSharedChannel(name) => name
     }
   }

    object SymbolMatcher {
      def unapply(t: Tree) = condOpt(t) {
        case Apply1(_, StringLit(role)) if t.symbol == symbolApplyMethod => role
      }
    }
    
    object SymbolUnapply {
      def unapply(t: Tree) = condOpt(t) {
        case UnApply1(m, StringLit(name)) if m.symbol == symbolUnapplyMethod => name
      }
    }

    object StringLit {
      def unapply(t: Tree) = condOpt(t) {
        case Literal(role) => role.stringValue
      }
    }

    object SessionRole {
      def unapply(t: Tree) = condOpt(t) {
        case Apply(Select(Ident(session), _),
                   SymbolMatcher(role)::Nil) => (session, role)
      }
    }

    def isTupleTpt(tpt: Tree) = definitions.isTupleType(tpt.symbol.tpe)

    // todo: figure out better names for all these variants
    def getSessionChannels(tuple: Tree): List[Name] = tuple match {
      case Apply(Select(New(tpt),nme.CONSTRUCTOR),args) if isTupleTpt(tpt) =>
        getSessionChannels(args)
      case SessionChannel(name) => List(name)
      case _ => Nil
    }

    def getSessionChannels(args: List[Tree]): List[Name] =
      args.map(SessionChannel.unapply).flatten

    def hasSessionChannels(args: List[Tree]) = !getSessionChannels(args).isEmpty
    def hasSessionChannels(tree: Tree) = !getSessionChannels(tree).isEmpty

    // nasty hack necessary to get correct type both for class instances and objects (aka modules)
    def realType(t: Type): Type = definitions.getClass(t.typeSymbol.fullName).tpe
    def getType(arg: Tree): Option[Type] = Some(realType(arg.tpe))

    def getLabelAndArgType(arg: Tree): (Option[String], Option[Type]) = {
      //println("arg.symbol.tpe: " + (if (arg.symbol == null) "(symbol null)" else arg.symbol.tpe))
      val ret = arg match {
        // The first pattern might be too broad, as it doesn't check that the outer Apply
        // is for a Tuple constructor
        case Apply(_, List(SymbolMatcher(label), arg)) =>
          (Some(label), getType(arg))
        case arg =>
          // This is the best way I've found as a workaround to a bug in the Scala compiler:
          // http://lampsvn.epfl.ch/trac/scala/ticket/1697
          // http://lampsvn.epfl.ch/trac/scala/ticket/2337
          // Explanations about the bug:
          // http://permalink.gmane.org/gmane.comp.lang.scala.internals/2752
          // http://scala-programming-language.1934581.n4.nabble.com/Possible-scala-partial-function-pattern-matching-bug-td3029632.html#none
          arg match {
            case SymbolMatcher(label) =>
              (Some(label), None)
            case _ =>
              (None, getType(arg))
          }
      }
      //println("getLabelAndArgType, returning: " + ret)
      assert(ret._1.isDefined || ret._2.get != definitions.getClass("scala.Symbol").tpe)
      ret
    }

    def tupleArity(tupleSym: Symbol): Int = {
      // name is TupleXX, XX starts at index 5
      val arityStr = tupleSym.name.toString.substring(5)
      Integer.parseInt(arityStr)
    }
    def indexAccessor(tupleAccessor: Name): Int =
      Integer.parseInt(tupleAccessor.toString.substring(1))

    def isTupleMethod(sel: Tree) = definitions.isTupleType(sel.symbol.owner.tpe)
    // Required for multiple-return session method calls, as the call desugars into
    // several valdefs
    private var syntheticValName: Name = null
    private var collectedRetVals: List[Name] = Nil
    private var sessionMethodSym: Symbol = null
    private var sessionMethodArgs: List[Tree] = Nil

    object RoleArrow {
      def unapply(t: Tree): Option[(String,Tree)] = condOpt(t) {
        case Apply1(TypeApply(Select(Apply(_,Apply1(_,StringLit(role))::_),_), _), arg) => 
          (role, arg)
      }
    }
    
    override def traverse(tree: Tree) {
      val sym = tree.symbol
      pos = tree.pos

      try {
        tree match {
          // message send. Without label: s ! 'Buyer -> 2000
          // with label: s ! 'Buyer -> ('Quote, 2000)
          // or: s ! 'Buyer -> 'Quote
          case Apply1(SessionIdent(session), RoleArrow(role, arg))
          if sym == bangMethod =>
            //println("!!!!!!! bangMethod, arg: " + arg + ", arg.tpe: " + arg.tpe + ", session: " + session + ", role: " + role)
            val (lbl,tpe) = getLabelAndArgType(arg)
            env = env.send(session, role, MsgSig(lbl, tpe))
            traverse(arg)
          
          // message receive, no label: s.?[Int]('Alice)
          case Apply1(TypeApply(SessionIdent(session), _), SymbolMatcher(role))
          if qmarkMethods.alternatives.contains(sym) =>
            if (tree.tpe == definitions.getClass("scala.Nothing").tpe)
              reporter.error(tree.pos, "Method ? needs to be annotated with explicit type")
            //println("????? qmarkMethod, tree.tpe:"+tree.tpe+", session: "+session+", role: "+role)
            env = env.receive(session, role, sig(tree.tpe))
            super.traverse(tree)

          // message receive, with label: s.?[Int]('Alice, 'label)
          case Apply(TypeApply(SessionIdent(session), _), SymbolMatcher(role)::SymbolMatcher(label)::Nil)
          if qmarkMethods.alternatives.contains(sym) =>
            println("?????????? message receive, session: " + session + ", role: "
                    + role+", msg type: " + sym.tpe + ", label: " + label)
            val unitSymbol = definitions.getClass("scala.Unit")
            val tpe = 
              if (tree.tpe == unitSymbol.tpe) None
              else Some(tree.tpe)
            env = env.receive(session, role, MsgSig(Some(label), tpe))

          // split match into 2 to work around bug #1133
          // https://lampsvn.epfl.ch/trac/scala/ticket/1133
          case x => x match {
              
            // receive/react: single role
            // s.receive('Alice) { case i: Int => ... case 'label => ... }  
            case Apply1(
                   Apply1(TypeApply(SessionIdent(session), _),
                          SymbolMatcher(role)),
                   f@Function(_,Match(_,cases)))
            if (sym == receiveMethod || sym == reactMethod) =>
              //println("receive/react, session: " + session + ", role: " + role
              //        + ", cases: " + cases)
              visitCases(session, Some(role), cases, f) { msgMatcher => {
                case x => msgMatcher(None)(x)
              }}

            // mreceive/mreact
            case Apply1(
                   TypeApply(SessionIdent(session), _),
                   f@Function(_,Match(_,cases)))
            if (sym == mreceiveMethod || sym == mreactMethod) =>
              //println("mreceive/mreact: "+tree)
              visitCases(session, None, cases, f) { msgMatcher => {
                case Apply(arrowConstr, SymbolUnapply(srcRole)::msg::Nil) =>
                  msgMatcher(Some(srcRole))(msg)
              }}
              
            // delegation, more than 1 session channel. Desugars into match + assignments to each channel val
            // 1. actual method call and match statement
            case ValDef(mods, synValName, tpt, Match(Typed(app@Apply(_,args),_),
              CaseDef(_, _, Apply(tupleConstructor, listArgs))::Nil))
            if sym.isSynthetic && hasSessionChannels(args) =>
              syntheticValName = synValName
              sessionMethodSym = app.symbol
              sessionMethodArgs = args
              //println("@@@@@@@@@ found synthetic valdef: " + synValName)
  
            // 2. valdef of each returned channel
            case ValDef(_, valName, tpt, sel@Select(Ident(selName), tupleAccessor))
            if syntheticValName != null && selName == syntheticValName && isSessionChannelType(sym.tpe) =>
              //println("$$$$$$$$$ found valdef for returned channel " + valName)
              collectedRetVals = valName :: collectedRetVals
              // test whether this is the last accessor - then we have seen all generated valdefs
              if (tupleArity(sel.symbol.owner) == indexAccessor(tupleAccessor)) {
                env = env.delegation(sessionMethodSym, getSessionChannels(sessionMethodArgs), collectedRetVals.reverse)
                syntheticValName = null
                collectedRetVals = Nil
              }
  
            // delegation returning 1 session channel
            case ValDef(_, valName, tpt, app@Apply(fun, args)) if hasSessionChannels(args) =>
              //println("delegation returning channel: " + valName + ", method call: " + app)
              env = env.delegation(fun.symbol, getSessionChannels(args), List(valName))
              super.traverse(app)
  
            // delegation, no returned channels (order of cases is important, will match if moved before previous case)
            case Apply(fun,args) if hasSessionChannels(args)
                    // tuples are used to return session channels from inside session methods
                    && !definitions.isTupleType(sym.owner.tpe) =>
              //println("delegation of session channel: " + tree)
              env = env.delegation(fun.symbol, getSessionChannels(args), Nil)
              super.traverse(tree)
  
            // todo: support pattern matching on standard receives, checking that all
            // cases are subtypes of protocol-defined type. (Maybe: enforce complete match?)
  
            case Assign(lhs,rhs@Channel(_)) => linearityError(lhs,rhs)
            // ValDef actually covers both val and var definitions
            case ValDef(_,name,_,rhs@Channel(_)) => linearityError(name,rhs)
  
            case If(cond,thenp,elsep) =>
              pos = thenp.pos
              //println("enterThen")
              env = env.enterThen
              //println("after enterThen, traversing body")
              traverse(thenp)
              pos = elsep.pos
              //println("enterElse")
              env = env.enterElse
              //println("after enterElse, traversing body")
              traverse(elsep)
              //println("leaveIf")
              env = env.leaveIf
              //println("after leaveIf")
  
            case DefDef(_,name,tparams,_,_,rhs) =>
              //println("method def: " + name + ", symbol: " + tree.symbol)
              val chanNames = sessionChannelNames(tree.symbol.tpe)
              val sharedChanNames = sharedChannelNames(tree.symbol.tpe)
              if (!sharedChanNames.isEmpty) {
                throw new SessionTypeCheckingException(
                  "Shared channels cannot be used as method parameters")
              } else if (!chanNames.isEmpty) {
                if (!tparams.isEmpty) reporter.error(tree.pos,
                  "Type parameters not supported for session methods")
                visitSessionMethod(tree.symbol, rhs, chanNames)
              } else {
                super.traverse(tree)
              }
  
            case LabelDef(_,_,block) =>
              env = env.enterLoop
              traverse(block)
              env = env.leaveLoop
  
            case Function(valdefs, body) =>
              env = env.enterClosure(getDefNames(valdefs))
              traverse(body)
              env = env.leaveClosure
  
            // return statements are forbidden inside join blocks / session methods
            // this could be relaxed in some cases, but added complexity doesn't seem to be worth it for now
            case Return(_) =>
              env = env.returnStatement
  
            case _ =>
              super.traverse(tree)
          }
        }

      } catch {
        case rex: RecoverableTypeCheckingException =>
          env = rex.recoveryEnv
          reporter.error(pos, rex.getMessage)
        case e: SessionTypeCheckingException =>
          reporter.error(pos, e.getMessage)
          throw e
      }
    }

    def getDefNames(valdefs: List[ValDef]): List[Name] = valdefs map {
      case ValDef(_, name, _, _) => name
    }

    def visitCases(sessChan: Name, srcRole: Option[String], cases: List[CaseDef], f: Tree)(matchFun: (Option[String] => PartialFunction[Tree,Unit]) => PartialFunction[Tree, Unit]) {
      env = env.enterChoiceReceiveBlock(sessChan, srcRole)
      cases foreach { c: CaseDef =>
        if (! c.guard.isEmpty) {
          reporter.error(c.guard.pos,
            "Receive clauses on session channels (branching) do not support guards yet")
        } else {
          val msgMatcher: Option[String] => PartialFunction[Tree, Unit] = { src =>
            val realSrc = srcRole.orElse(src) ;      
            { x => x match {
              // Bind is for case s: String
              // Ident and Select both needed to use objects:
              // case OK is Ident
              // case pkg.foo.OK is Select
              // fixme: make sure Select and Ident don't fit other kinds of patterns that will not always match,
              // breaking typesafety
              case Select(_,_) | Ident(_) | Bind(_,_) =>
                val t = x.tpe
                val msgTpe = realType(t)
                visitBranch(c, realSrc, sig(msgTpe))
  
              case SymbolUnapply(label) =>
                visitBranch(c, realSrc, sig(label))
  
              case Apply(tpt, List(SymbolUnapply(label), bind:Bind)) =>
                visitBranch(c, realSrc, sig(label, bind.symbol.tpe))

              case _ =>
                reporter.error(x.pos,
                  "Receive clauses on session channels (branching) do not support complex patterns yet")          
            }}
          }
          val default: PartialFunction[Tree, Unit] = { case x =>
            reporter.error(x.pos, "Incorrect session branch receive pattern")
            //println("no match: "+x)
          } 
          (matchFun(msgMatcher) orElse default)(c.pat)
        }
      }
      pos = f.pos
      env = env.leaveChoiceReceiveBlock
    }
    
    def visitBranch(c: CaseDef, src: Option[String], msgSig: MsgSig) {
      pos = c.pat.pos
      env = env.enterChoiceReceiveBranch(src, msgSig)
      pos = c.body.pos
      traverse(c.body)
      pos = c.body.pos
      env = env.leaveChoiceReceiveBranch
    }

    def visitSessionMethod(method: Symbol, body: Tree, chanNames: List[Name])

    def isSessionChannelType(t: Type): Boolean = {
      val sessionChannel = sessionChannelClass.tpe
      //println(t + " <:< " + sessionChannel + ": " + (t <:< sessionChannel))
      t <:< sessionChannel
    }

    def sessionChannelNames(tpe: Type): List[Name] = chanNames(tpe, isSessionChannelType(_))

    def sharedChannelNames(tpe: Type): List[Name] = chanNames(tpe, (_ <:< addressTrait.tpe))

    def chanNames(tpe: Type, predicate: (Type => Boolean)) = tpe match {
      case MethodType(argTypes, _) =>
        (for (argS <- argTypes if predicate(argS.tpe))
          yield Some(argS.name)) flatten
      case _ => Nil
    }
  }
}