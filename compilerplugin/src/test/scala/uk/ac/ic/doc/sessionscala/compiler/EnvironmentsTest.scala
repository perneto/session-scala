package uk.ac.ic.doc.sessionscala.compiler

import org.scalatest.FunSuite
import org.scribble.protocol.model._
import org.scalatest.matchers.ShouldMatchers

/**
 * Created by: omp08
 */

class EnvironmentsTest extends FunSuite with SessionTypingEnvironments
                                        with ScalaCompilerSetup
                                        with ScribbleParsing
                                        with ShouldMatchers {

  import global._

  val topEnv = new JoinBlocksPassTopLevelEnv
  val sharedChan = newTermName("sharedChannel")
  val sharedChan2 = newTermName("sharedChan2")
  val sessChan = newTermName("sessionChannel")
  val sessChan2 = newTermName("sessChan2")
  val stringT = definitions.StringClass.tpe
  val intT = definitions.IntClass.tpe
  val floatT = definitions.FloatClass.tpe
  val objectT = definitions.ObjectClass.tpe
  val anyT = definitions.AnyClass.tpe
  val charSequenceT = definitions.getClass("java.lang.CharSequence").tpe
  
  val stringTRef = ScalaTypeReference(stringT)
  val charSequenceTRef = ScalaTypeReference(charSequenceT)
  val intTRef = ScalaTypeReference(intT)
  val floatTRef = ScalaTypeReference(floatT)
  val objectTRef = ScalaTypeReference(objectT)
  val aliceRole = new Role("Alice")
  val bobRole = new Role("Bob")

  def createInteraction(src: Role, dst: Role, msgType: TypeReference) =
      new Interaction(src, dst, new MessageSignature(msgType))

  def join(model: ProtocolModel, joinAs: String): SessionTypingEnvironment = {
    val env = topEnv.registerSharedChannel(sharedChan, model)
    env.enterJoin(sharedChan, joinAs, sessChan)
  }

  val empty = definitions.EmptyPackage
  val fooMethod = empty.newMethod(mkTermName("foo"))

  def sessionMethod(method: Symbol, chans: Name*): SessionTypingEnvironment = {
    var env: SessionTypingEnvironment = new MethodSessionTypeInferenceTopLevelEnv
    env.enterSessionMethod(method, List(chans: _*))
  }

  def inferred(env: SessionTypingEnvironment, method: Symbol, rank: Int): LabelledBlock =
    env.asInstanceOf[MethodSessionTypeInferenceTopLevelEnv]
                                    .inferredSessionType(method, rank)

  def checkInferred(env: SessionTypingEnvironment, meth: Symbol, chanRank: Int, label: String, block: List[Activity]) {
    val inf = inferred(env, meth, chanRank)
    val expected = createLabelledBlock(label, block)
    assert(inf === expected)
  }

  test("top-level enter join, unregistered channel: error") {
    intercept[SessionTypeCheckingException] {
      topEnv.enterJoin(sharedChan, "A", sessChan)
    }
  }

  val emptyProtoModel = parse(
  """protocol Foo { role Alice; }
  """)

  test("enter and leave join, empty protocol") {
    var env = join(emptyProtoModel, "Alice")
    env = env.leaveJoin
  }

  test("unexpected send, empty protocol: error") {
    var env = join(emptyProtoModel, "Alice")
    intercept[SessionTypeCheckingException] {
      env = env.send(sessChan, "Alice", sig(stringT))
    }
  }

  val sendStringModel = parse(
    """protocol Foo {
         role Alice, Bob;
         String from Alice to Bob;
       }
    """)

  test("basic protocol, complete") {
    var env = join(sendStringModel, "Alice")
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.leaveJoin
  }

  test("basic protocol, wrong message type: error") {
    var env = join(sendStringModel, "Alice")
    intercept[SessionTypeCheckingException] {
      env = env.send(sessChan, "Bob", sig(objectT)) // wrong message type
    }
  }

  test("basic protocol, missing interaction: error") {
    var env = join(sendStringModel, "Alice")
    // missing send
    intercept[SessionTypeCheckingException] {
      env = env.leaveJoin
    }
  }

  test("basic protocol, receive side") {
    var env = join(sendStringModel, "Bob")
    env = env.receive(sessChan, "Alice", sig(stringT))
    env = env.leaveJoin
  }

  val choiceProtoModel = parse(
    """protocol Foo {
         role Alice, Bob;
         choice from Alice to Bob {
           String {}
           Int {}
         }
       }
    """)

  test("protocol with choice, chooser side, complete") {
    var env = join(choiceProtoModel, "Alice")
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.leaveJoin
  }

  test("protocol with choice, receiver side, complete") {
    var env = join(choiceProtoModel, "Bob")
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(sig(stringT))
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(sig(intT))
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    env = env.leaveJoin
  }


  test("choice, if branches on chooser side, complete") {
    var env = join(choiceProtoModel, "Alice")
    env = env.enterThen
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.enterElse
    env = env.send(sessChan, "Bob", sig(intT))
    env = env.leaveIf
    env = env.leaveJoin
  }

  test("choice, supertype on receive covers 2 branches: error")  {
    var env = join(choiceProtoModel, "Bob")
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(sig(anyT))
    env = env.leaveChoiceReceiveBranch
    intercept[SessionTypeCheckingException] {
      env = env.leaveChoiceReceiveBlock
    }
  }

  test("choice, if branches on receive side, branch label is supertype but covers single branch, complete") {
    var env = join(choiceProtoModel, "Bob")
    env = env.enterThen
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(sig(stringT))
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(sig(intT))
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock

    env = env.enterElse
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(sig(charSequenceT))
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(sig(intT))
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
        
    env = env.leaveIf
    env = env.leaveJoin
  }

  val twoMsgProto = parse(
  """protocol Foo {
       role Alice, Bob;
       String from Alice to Bob;
       Int from Bob to Alice;
     }
  """)

  test("interleaved sessions") {
    var env = join(twoMsgProto, "Alice")
    env = env.registerSharedChannel(sharedChan2, twoMsgProto)
    env = env.enterJoin(sharedChan2, "Bob", sessChan2)
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.receive(sessChan2, "Alice", sig(stringT))
    env = env.send(sessChan2, "Alice", sig(intT))
    env = env.receive(sessChan, "Bob", sig(intT))
    env = env.leaveJoin
    env = env.leaveJoin
  }

  test("interleaved sessions, branches receive side") {
    var env = join(choiceProtoModel, "Bob")

    env = env.registerSharedChannel(sharedChan2, twoMsgProto)
    env = env.enterJoin(sharedChan2, "Alice", sessChan2)
    env = env.send(sessChan2, "Bob", sig(stringT))

    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(sig(stringT))

    env = env.receive(sessChan2, "Bob", sig(intT))

    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(sig(intT))

    env = env.receive(sessChan2, "Bob", sig(intT))

    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock

    env = env.leaveJoin
    env = env.leaveJoin
  }
  
  test("method inference, one send and receive") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.receive(sessChan, "Bob", sig(stringT))
    env = env.leaveSessionMethod(Nil)
    
    // all inferred methods are inferred as potentially recursive,
    // and use their name as recursion variable. (todo: later need to allow for overloading)
    checkInferred(env, fooMethod, 0, "X1", List(
        createInteraction(null, bobRole, stringTRef),
        createInteraction(bobRole, null, stringTRef)
    ))
  }

  test("method inference, branching, common part before branching") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.enterChoiceReceiveBlock(sessChan, "Bob")
    env = env.enterChoiceReceiveBranch(sig(stringT))
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(sig(intT))
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    env = env.leaveSessionMethod(Nil)

    checkInferred(env, fooMethod, 0, "X1", List(
        createInteraction(null, bobRole, stringTRef),
        createChoice(bobRole, null, emptyBody(List(stringTRef, intTRef)))
    ))
  }

  test("method inference, branching, non-empty body, nothing before branch") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.enterChoiceReceiveBlock(sessChan, "Bob")
    env = env.enterChoiceReceiveBranch(sig(stringT))
    env = env.send(sessChan, "Alice", sig(intT))
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(sig(intT))
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    env = env.leaveSessionMethod(Nil)

    checkInferred(env, fooMethod, 0, "X1", List(
        createChoice(bobRole, null, List(
            (stringTRef, List(
              createInteraction(null, aliceRole, intTRef)
              )),
            (intTRef, Nil)
          ))
    ))
  }
  
  test("method inference, if branches should be merged into choice") {
    var env = sessionMethod(fooMethod, sessChan)

    env = env.send(sessChan, "Alice", sig(objectT))

    env = env.enterThen
    env = env.send(sessChan, "Alice", sig(stringT))
    env = env.receive(sessChan, "Alice", sig(floatT))
    env = env.enterElse
    env = env.send(sessChan, "Alice", sig(intT))
    env = env.leaveIf
    env = env.leaveSessionMethod(Nil)
    
    checkInferred(env, fooMethod, 0, "X1", List(
        createInteraction(null, aliceRole, objectTRef),
        createChoice(null, aliceRole, List(
            (stringTRef, List(createInteraction(aliceRole, null, floatTRef))),
            (intTRef, Nil)))
    ))
  }

  test("method inference, more than 2 if branches, merge into choice") {
    var env = sessionMethod(fooMethod, sessChan)

    env = env.enterThen
    env = env.send(sessChan, "Alice", sig(stringT))
    env = env.enterElse
    env = env.enterThen
    env = env.send(sessChan, "Alice", sig(intT))
    env = env.enterElse
    env = env.send(sessChan, "Alice", sig(floatT))
    env = env.leaveIf
    env = env.leaveIf
    env = env.leaveSessionMethod(Nil)

    checkInferred(env, fooMethod, 0, "X1", List(
        // todo: right now whens are in a list so Choice.equals is order-sensitive. need to change it to a set
        createChoice(null, aliceRole, emptyBody(List(intTRef, floatTRef, stringTRef)))
    ))
  }

  test("method inference, if branches, second branch doesn't send label: error") {
    var env = sessionMethod(fooMethod, sessChan)

    env = env.enterThen
    env = env.send(sessChan, "Alice", sig(stringT))
    env = env.enterElse
    intercept[SessionTypeCheckingException] {
      env = env.leaveIf
    }
  }

  test("method inference, if branches, first branch doesn't send label: error") {
    var env = sessionMethod(fooMethod, sessChan)

    env = env.enterThen
    env = env.enterElse
    env = env.send(sessChan, "Alice", sig(stringT))
    intercept[SessionTypeCheckingException] {
      env = env.leaveIf
    }
  }

  test("method inference, if branches, identical: no choice") {
    var env = sessionMethod(fooMethod, sessChan)

    env = env.enterThen
    env = env.send(sessChan, "Alice", sig(intT))
    env = env.enterElse
    env = env.send(sessChan, "Alice", sig(intT))
    env = env.leaveIf
    env = env.leaveSessionMethod(Nil)

    checkInferred(env, fooMethod, 0, "X1", List(
        createInteraction(null, aliceRole, intTRef)
    ))
  }

  test("method inference, if branches, identical modulo subtyping: no choice") {
    var env = sessionMethod(fooMethod, sessChan)

    env = env.enterThen
    env = env.send(sessChan, "Alice", sig(stringT))
    env = env.receive(sessChan, "Alice", sig(stringT))
    env = env.enterElse
    env = env.send(sessChan, "Alice", sig(charSequenceT))
    env = env.receive(sessChan, "Alice", sig(charSequenceT))
    env = env.leaveIf
    env = env.leaveSessionMethod(Nil)

    checkInferred(env, fooMethod, 0, "X1", List(
        createInteraction(null, aliceRole, charSequenceTRef), // send: infer supertype
        createInteraction(aliceRole, null, stringTRef) // receive: infer subtype
    ))
  }
  
  test("method inference, recursion") {
    var env = sessionMethod(fooMethod, sessChan)
    
    env = env.send(sessChan, "Alice", sig(stringT))
    env = env.delegation(fooMethod, List(sessChan), Nil)
    env = env.leaveSessionMethod(Nil)
    
    checkInferred(env, fooMethod, 0, "X1", List(
        createInteraction(null, aliceRole, stringTRef),
        createRecursion("X1")
    ))
  }

  test("method inference, interleaved sessions, basic send-receive") {
    var env = sessionMethod(fooMethod, sessChan, sessChan2)
    
    env = env.send(sessChan, "Alice", sig(stringT))
    env = env.receive(sessChan2, "Bob", sig(intT))
    env = env.leaveSessionMethod(Nil)

    checkInferred(env, fooMethod, 0, "X1", List (
      createInteraction(null, aliceRole, stringTRef)
    ))
    checkInferred(env, fooMethod, 1, "X1", List (
      createInteraction(bobRole, null, intTRef)
    ))
  }

  test("method inference, interleaved sessions, if branches") {
    var env = sessionMethod(fooMethod, sessChan, sessChan2)
    
    env = env.enterThen
    env = env.send(sessChan, "Alice", sig(intT))
    env = env.receive(sessChan2, "Bob", sig(intT))

    env = env.enterElse
    env = env.send(sessChan, "Alice", sig(stringT))
    env = env.receive(sessChan2, "Bob", sig(intT))

    env = env.leaveIf
    env = env.leaveSessionMethod(Nil)

    checkInferred(env, fooMethod, 0, "X1", List (
      createChoice(null, aliceRole, emptyBody(List(intTRef, stringTRef)))
    ))
    checkInferred(env, fooMethod, 1, "X1", List (
      createInteraction(bobRole, null, intTRef)
    ))
  }

  test("method inference, interleaved sessions, choice branches") {
    var env = sessionMethod(fooMethod, sessChan, sessChan2)
    
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.receive(sessChan2, "Alice", sig(intT))

    env = env.enterChoiceReceiveBlock(sessChan, "Bob")

    env = env.enterChoiceReceiveBranch(sig(stringT))
    env = env.receive(sessChan2, "Bob", sig(intT))
    env = env.leaveChoiceReceiveBranch

    env = env.enterChoiceReceiveBranch(sig(intT))
    env = env.receive(sessChan2, "Bob", sig(intT))
    env = env.leaveChoiceReceiveBranch

    env = env.leaveChoiceReceiveBlock
    env = env.leaveSessionMethod(Nil)

    checkInferred(env, fooMethod, 0, "X1", List(
      createInteraction(null, bobRole, stringTRef),
      createChoice(bobRole, null, emptyBody(List(stringTRef, intTRef)))
    ))
    checkInferred(env, fooMethod, 1, "X1", List (
      createInteraction(aliceRole, null, intTRef),
      createInteraction(bobRole, null, intTRef)
    ))
  }

  test("method inference, interleaved sessions, choice branches, uneven interleaved session") {
    var env = sessionMethod(fooMethod, sessChan, sessChan2)
    
    env = env.enterChoiceReceiveBlock(sessChan, "Bob")

    env = env.enterChoiceReceiveBranch(sig(stringT))
    env = env.receive(sessChan2, "Bob", sig(intT))
    env = env.leaveChoiceReceiveBranch

    env = env.enterChoiceReceiveBranch(sig(intT))
    env = env.receive(sessChan2, "Bob", sig(intT))
    env = env.receive(sessChan2, "Bob", sig(intT))

    intercept[SessionTypeCheckingException] {
      env = env.leaveChoiceReceiveBranch
      env = env.leaveChoiceReceiveBlock
    }
  }

  test("inferred method call, basic send/receive proto") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.leaveSessionMethod(Nil)
    
    env = new JoinBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, sendStringModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.delegation(fooMethod, List(sessChan), Nil)
    env = env.leaveJoin
  }

  test("inferred method call, choice proto, receive side") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(sig(stringT))
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(sig(intT))
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    env = env.leaveSessionMethod(Nil)

    env = new JoinBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, choiceProtoModel)
    env = env.enterJoin(sharedChan, "Bob", sessChan)
    env = env.delegation(fooMethod, List(sessChan), Nil)
    env = env.leaveJoin
  }

  test("inferred method call, choice proto, wrong role: error") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.enterChoiceReceiveBlock(sessChan, "Foo")
    env = env.enterChoiceReceiveBranch(sig(stringT))
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(sig(intT))
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    env = env.leaveSessionMethod(Nil)

    env = new JoinBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, choiceProtoModel)
    env = env.enterJoin(sharedChan, "Bob", sessChan)
    intercept[SessionTypeCheckingException] {
      env = env.delegation(fooMethod, List(sessChan), Nil)
    }
  }

  test("inferred method call, choice proto, wrong body of when branch: error") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(sig(stringT))
    env = env.send(sessChan, "Bar", sig(intT)) // not in protocol: error
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(sig(intT))
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    env = env.leaveSessionMethod(Nil)

    env = new JoinBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, choiceProtoModel)
    env = env.enterJoin(sharedChan, "Bob", sessChan)
    intercept[SessionTypeCheckingException] {
      env = env.delegation(fooMethod, List(sessChan), Nil)
    }
  }

  test("inferred method call, choice proto, send side") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.enterThen
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.enterElse
    env = env.send(sessChan, "Bob", sig(intT))
    env = env.leaveIf
    env = env.leaveSessionMethod(Nil)

    env = new JoinBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, choiceProtoModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.delegation(fooMethod, List(sessChan), Nil)
    env = env.leaveJoin
  }

  test("inferred method call, choice proto, send side, wrong label: error") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.enterThen
    env = env.send(sessChan, "Bob", sig(objectT)) // should be stringT -> error
    env = env.enterElse
    env = env.send(sessChan, "Bob", sig(intT))
    env = env.leaveIf
    env = env.leaveSessionMethod(Nil)

    env = new JoinBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, choiceProtoModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    intercept[SessionTypeCheckingException] {
      env = env.delegation(fooMethod, List(sessChan), Nil)
    }
  }

  val recurModel = parse(
  """protocol Foo {
       role Alice, Bob;
       X: {
         String from Alice to Bob;
         #X;
       }
     }
  """)
  
  test("inferred, recursive method call - unroll recursion first") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.delegation(fooMethod, List(sessChan), Nil)
    env = env.leaveSessionMethod(Nil)
    
    env = new JoinBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, recurModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.delegation(fooMethod, List(sessChan), Nil)
    env = env.leaveJoin
  }

  val multiRecurModel = parse(
  """protocol Foo {
       role Alice, Bob;
       X: {
         //Int from Alice to Bob;
         Y: {
           choice from Alice to Bob {
             Int { #X; }
             String { #Y; }
           }
         }
       }
     }
  """)

  val xmethod = empty.newMethod(mkTermName("methodX"))
  val ymethod = empty.newMethod(mkTermName("methodY"))
  test("inferred, recursive method call, multiple recursion labels") {
    var env = sessionMethod(xmethod, sessChan)
    //env = env.send(sessChan, "Bob", sig(intT))
    env = env.delegation(ymethod, List(sessChan), Nil)
    env = env.leaveSessionMethod(Nil)
    assert(notEmpty(env.asInstanceOf[InferredTypeRegistry].inferredSessionType(xmethod, 0)))
    println("after assert")

    env = env.enterSessionMethod(ymethod, List(sessChan))
    env = env.enterThen
    env = env.send(sessChan, "Bob", sig(intT))
    env = env.delegation(xmethod, List(sessChan), Nil)
    env = env.enterElse
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.delegation(ymethod, List(sessChan), Nil)
    env = env.leaveIf
    env = env.leaveSessionMethod(Nil)

    assert(notEmpty(env.asInstanceOf[InferredTypeRegistry].inferredSessionType(xmethod, 0)))
    assert(notEmpty(env.asInstanceOf[InferredTypeRegistry].inferredSessionType(ymethod, 0)))

    env = new JoinBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, multiRecurModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.delegation(xmethod, List(sessChan), Nil)
    env = env.leaveJoin
  }
  	
  ignore("scoping of inferred methods") {
    // will require keeping even the join block visitor informed of method definitions,
    // so that it can bring previously inferred methods in and out of scope
    // or maybe the Symbol objects already have enough info?
  }

  test("linearity: cannot use session channel after passing it to a method") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.leaveSessionMethod(Nil)

    env = new JoinBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, sendStringModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.delegation(fooMethod, List(sessChan), Nil)
    intercept[SessionTypeCheckingException] {
      env = env.send(sessChan, "Bob", sig(stringT)) // reuse of sessChan not allowed
    }
  }

  test("returning a channel from session method") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.leaveSessionMethod(List(sessChan))

    env = new JoinBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, sendStringModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    val returnedSessChan = newTermName("returnedChan")
    env = env.delegation(fooMethod, List(sessChan), List(returnedSessChan))
    env = env.send(returnedSessChan, "Bob", sig(stringT))
    env = env.leaveJoin
  }

  test("inference: inferredtyperegistry has order of returned channels") {
    val s0 = newTermName("s0")
    val s1 = newTermName("s1")
    val s2 = newTermName("s2")
    val s3 = newTermName("s3")
    val s4 = newTermName("s4")

    var env = sessionMethod(fooMethod, s0, s1, s2, s3, s4)
    env = env.leaveSessionMethod(List(s1, s2, s0, s4))

    val reg = env.asInstanceOf[InferredTypeRegistry]
    assert(reg.returnRank(fooMethod, 0) === Some(2))
    assert(reg.returnRank(fooMethod, 1) === Some(0))
    assert(reg.returnRank(fooMethod, 2) === Some(1))
    assert(reg.returnRank(fooMethod, 3) === None)
    assert(reg.returnRank(fooMethod, 4) === Some(3))
  }

  test("inverted channel order when returning channels from session method") {
    val retChan = newTermName("returnedChan")
    val retChan2 = newTermName("returnedChan2")

    var env = sessionMethod(fooMethod, sessChan, sessChan2)
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.leaveSessionMethod(List(sessChan2, sessChan))

    env = new JoinBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, twoMsgProto)
    env = env.registerSharedChannel(sharedChan2, twoMsgProto)

    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.enterJoin(sharedChan, "Alice", sessChan2)

    env = env.delegation(fooMethod, List(sessChan, sessChan2), List(retChan2, retChan))

    env = env.send(retChan2, "Bob", sig(stringT))
    env = env.receive(retChan, "Bob", sig(intT))
    env = env.receive(retChan2, "Bob", sig(intT))

    env = env.leaveJoin
  }

  test("different channel names for formal and effective parameters to session method") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.leaveSessionMethod(Nil)

    env = new JoinBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, sendStringModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan2)
    env = env.delegation(fooMethod, List(sessChan2), Nil)
    env = env.leaveJoin
  }

  test("incomplete session on channel bound by session method return value") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.leaveSessionMethod(List(sessChan))

    env = new JoinBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, twoMsgProto)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.delegation(fooMethod, List(sessChan), List(sessChan2))
    intercept[SessionTypeCheckingException] {
      // sessChan2 still has Int from Bob to Alice
      env = env.leaveJoin
    }
  }

  test("forbid session operations on channel defined outside of loop") {
    var env = join(sendStringModel, "Alice")
    env = env.enterLoop
    intercept[SessionTypeCheckingException] {
      env = env.send(sessChan, "Bob", sig(stringT))
    }
  }


  test("session operations on channel defined inside loop are allowed") {
    var env = topEnv.registerSharedChannel(sharedChan, sendStringModel)
    env = env.enterLoop
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.leaveJoin
    env.leaveLoop
  }
}
