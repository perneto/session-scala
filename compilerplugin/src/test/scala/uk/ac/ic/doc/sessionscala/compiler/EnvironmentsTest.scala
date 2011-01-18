package uk.ac.ic.doc.sessionscala.compiler

import org.scalatest.FunSuite
import org.scribble.protocol.model._
import org.scalatest.matchers.ShouldMatchers

/**
 * Created by: omp08
 */

class EnvironmentsTest extends FunSuite with SessionTypingEnvironments
                                        with ScalaTypeSystemComponent
                                        with ScribbleModelFactories
                                        with SessionTypedElementsComponent
                                        with ScalaCompilerSetup
                                        with ScribbleParsing
                                        with ShouldMatchers {

  import global._

  val topEnv = new JoinBlockTopLevelEnv
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

  def join(model: ProtocolModel, joinAs: String): SessionTypingEnvironment = {
    val env = topEnv.registerSharedChannel(sharedChan, model)
    env.enterJoin(sharedChan, joinAs, sessChan)
  }

  val empty = definitions.EmptyPackage
  val fooMethod = empty.newMethod(mkTermName("foo"))

  def sessionMethod(method: Symbol, chan: Name): SessionTypingEnvironment = {
    var env: SessionTypingEnvironment = new MethodSessionTypeInferenceTopLevelEnv
    env.enterSessionMethod(method, List(chan))
  }

  def inferred(env: SessionTypingEnvironment, method: Symbol, chan: Name): LabelledBlock =
    env.asInstanceOf[MethodSessionTypeInferenceTopLevelEnv]
                                    .inferredSessionType(method, chan)

  def checkInferred(env: SessionTypingEnvironment, meth: Symbol, chan: Name, label: String, block: List[Activity]) {
    val inf = inferred(env, meth, chan)
    val expected = createLabelledBlock(label, block)
    assert(inf === expected)
  }

  def sessionMethod(method: Symbol, chan1: Name, chan2: Name): SessionTypingEnvironment = {
    val env: SessionTypingEnvironment = new MethodSessionTypeInferenceTopLevelEnv
    env.enterSessionMethod(method, List(chan1, chan2))
  }    


  test("top-level enter join, unregistered channel: error") {
    intercept[SessionTypeCheckingException] {
      topEnv.enterJoin(sharedChan, "A", sessChan)
    }
  }

  test("top-level leave (no session to be left): error") {
    intercept[SessionTypeCheckingException] {
      topEnv.leaveJoin
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
      env = env.send(sessChan, "Alice", stringT)
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
    env = env.send(sessChan, "Bob", stringT)
    env = env.leaveJoin
  }

  test("basic protocol, wrong message type: error") {
    var env = join(sendStringModel, "Alice")
    intercept[SessionTypeCheckingException] {
      env = env.send(sessChan, "Bob", objectT) // wrong message type
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
    env = env.receive(sessChan, "Alice", stringT)
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
    env = env.send(sessChan, "Bob", stringT)
    env = env.leaveJoin
  }

  test("protocol with choice, receiver side, complete") {
    var env = join(choiceProtoModel, "Bob")
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(stringT)
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(intT)
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    env = env.leaveJoin
  }


  test("choice, if branches on chooser side, complete") {
    var env = join(choiceProtoModel, "Alice")
    env = env.enterThen
    env = env.send(sessChan, "Bob", stringT)
    env = env.enterElse
    env = env.send(sessChan, "Bob", intT)
    env = env.leaveIf
    env = env.leaveJoin
  }

  test("choice, supertype on receive covers 2 branches: error")  {
    var env = join(choiceProtoModel, "Bob")
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(anyT)
    env = env.leaveChoiceReceiveBranch
    intercept[SessionTypeCheckingException] {
      env = env.leaveChoiceReceiveBlock
    }
  }

  test("choice, if branches on receive side, branch label is supertype but covers single branch, complete") {
    var env = join(choiceProtoModel, "Bob")
    env = env.enterThen
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(stringT)
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(intT)
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock

    env = env.enterElse
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(charSequenceT)
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(intT)
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
    env = env.send(sessChan, "Bob", stringT)
    env = env.receive(sessChan2, "Alice", stringT)
    env = env.send(sessChan2, "Alice", intT)
    env = env.receive(sessChan, "Bob", intT)
    env = env.leaveJoin
    env = env.leaveJoin
  }

  test("interleaved sessions, branches receive side") {
    var env = join(choiceProtoModel, "Bob")

    env = env.registerSharedChannel(sharedChan2, twoMsgProto)
    env = env.enterJoin(sharedChan2, "Alice", sessChan2)
    env = env.send(sessChan2, "Bob", stringT)

    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(stringT)

    env = env.receive(sessChan2, "Bob", intT)

    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(intT)

    env = env.receive(sessChan2, "Bob", intT)

    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock

    env = env.leaveJoin
    env = env.leaveJoin
  }
  
  test("method inference, one send and receive") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.send(sessChan, "Bob", stringT)
    env = env.receive(sessChan, "Bob", stringT)
    env = env.leaveSessionMethod
    
    // all inferred methods are inferred as potentially recursive,
    // and use their name as recursion variable. (todo: later need to allow for overloading)
    checkInferred(env, fooMethod, sessChan, "X1", List(
        createInteraction(null, bobRole, stringTRef),
        createInteraction(bobRole, null, stringTRef)
    ))
  }

  test("method inference, branching, common part before branching") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.send(sessChan, "Bob", stringT)
    env = env.enterChoiceReceiveBlock(sessChan, "Bob")
    env = env.enterChoiceReceiveBranch(stringT)
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(intT)
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    env = env.leaveSessionMethod

    checkInferred(env, fooMethod, sessChan, "X1", List(
        createInteraction(null, bobRole, stringTRef),
        createChoice(bobRole, null, emptyBody(List(stringTRef, intTRef)))
    ))
  }

  test("method inference, branching, non-empty body, nothing before branch") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.enterChoiceReceiveBlock(sessChan, "Bob")
    env = env.enterChoiceReceiveBranch(stringT)
    env = env.send(sessChan, "Alice", intT)
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(intT)
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    env = env.leaveSessionMethod

    checkInferred(env, fooMethod, sessChan, "X1", List(
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

    env = env.send(sessChan, "Alice", objectT)

    env = env.enterThen
    env = env.send(sessChan, "Alice", stringT)
    env = env.receive(sessChan, "Alice", floatT)
    env = env.enterElse
    env = env.send(sessChan, "Alice", intT)
    env = env.leaveIf
    env = env.leaveSessionMethod
    
    checkInferred(env, fooMethod, sessChan, "X1", List(
        createInteraction(null, aliceRole, objectTRef),
        createChoice(null, aliceRole, List(
            (stringTRef, List(createInteraction(aliceRole, null, floatTRef))),
            (intTRef, Nil)))
    ))
  }

  test("method inference, more than 2 if branches, merge into choice") {
    var env = sessionMethod(fooMethod, sessChan)

    env = env.enterThen
    env = env.send(sessChan, "Alice", stringT)
    env = env.enterElse
    env = env.enterThen
    env = env.send(sessChan, "Alice", intT)
    env = env.enterElse
    env = env.send(sessChan, "Alice", floatT)
    env = env.leaveIf
    env = env.leaveIf
    env = env.leaveSessionMethod

    checkInferred(env, fooMethod, sessChan, "X1", List(
        // todo: right now whens are in a list so Choice.equals is order-sensitive. need to change it to a set
        createChoice(null, aliceRole, emptyBody(List(intTRef, floatTRef, stringTRef)))
    ))
  }

  test("method inference, if branches, second branch doesn't send label: error") {
    var env = sessionMethod(fooMethod, sessChan)

    env = env.enterThen
    env = env.send(sessChan, "Alice", stringT)
    env = env.enterElse
    intercept[SessionTypeCheckingException] {
      env = env.leaveIf
    }
  }

  test("method inference, if branches, first branch doesn't send label: error") {
    var env = sessionMethod(fooMethod, sessChan)

    env = env.enterThen
    env = env.enterElse
    env = env.send(sessChan, "Alice", stringT)
    intercept[SessionTypeCheckingException] {
      env = env.leaveIf
    }
  }

  test("method inference, if branches, identical: no choice") {
    var env = sessionMethod(fooMethod, sessChan)

    env = env.enterThen
    env = env.send(sessChan, "Alice", intT)
    env = env.enterElse
    env = env.send(sessChan, "Alice", intT)
    env = env.leaveIf
    env = env.leaveSessionMethod

    checkInferred(env, fooMethod, sessChan, "X1", List(
        createInteraction(null, aliceRole, intTRef)
    ))
  }

  test("method inference, if branches, identical modulo subtyping: no choice") {
    var env = sessionMethod(fooMethod, sessChan)

    env = env.enterThen
    env = env.send(sessChan, "Alice", stringT)
    env = env.receive(sessChan, "Alice", stringT)
    env = env.enterElse
    env = env.send(sessChan, "Alice", charSequenceT)
    env = env.receive(sessChan, "Alice", charSequenceT)
    env = env.leaveIf
    env = env.leaveSessionMethod

    checkInferred(env, fooMethod, sessChan, "X1", List(
        createInteraction(null, aliceRole, charSequenceTRef), // send: infer supertype
        createInteraction(aliceRole, null, stringTRef) // receive: infer subtype
    ))
  }
  
  test("method inference, recursion") {
    var env = sessionMethod(fooMethod, sessChan)
    
    env = env.send(sessChan, "Alice", stringT)
    env = env.delegation(fooMethod, List(sessChan)) 
    env = env.leaveSessionMethod
    
    checkInferred(env, fooMethod, sessChan, "X1", List(
        createInteraction(null, aliceRole, stringTRef),
        createRecursion("X1")
    ))
  }

  test("method inference, interleaved sessions, basic send-receive") {
    var env = sessionMethod(fooMethod, sessChan, sessChan2)
    
    env = env.send(sessChan, "Alice", stringT)
    env = env.receive(sessChan2, "Bob", intT)
    env = env.leaveSessionMethod

    checkInferred(env, fooMethod, sessChan, "X1", List (
      createInteraction(null, aliceRole, stringTRef)
    ))
    checkInferred(env, fooMethod, sessChan2, "X1", List (
      createInteraction(bobRole, null, intTRef)
    ))
  }

  test("method inference, interleaved sessions, if branches") {
    var env = sessionMethod(fooMethod, sessChan, sessChan2)
    
    env = env.enterThen
    env = env.send(sessChan, "Alice", intT)
    env = env.receive(sessChan2, "Bob", intT)

    env = env.enterElse
    env = env.send(sessChan, "Alice", stringT)
    env = env.receive(sessChan2, "Bob", intT)

    env = env.leaveIf
    env = env.leaveSessionMethod

    checkInferred(env, fooMethod, sessChan, "X1", List (
      createChoice(null, aliceRole, emptyBody(List(intTRef, stringTRef)))
    ))
    checkInferred(env, fooMethod, sessChan2, "X1", List (
      createInteraction(bobRole, null, intTRef)
    ))
  }

  test("method inference, interleaved sessions, choice branches") {
    var env = sessionMethod(fooMethod, sessChan, sessChan2)
    
    env = env.send(sessChan, "Bob", stringT)
    env = env.receive(sessChan2, "Alice", intT)

    env = env.enterChoiceReceiveBlock(sessChan, "Bob")

    env = env.enterChoiceReceiveBranch(stringT)
    env = env.receive(sessChan2, "Bob", intT)
    env = env.leaveChoiceReceiveBranch

    env = env.enterChoiceReceiveBranch(intT)
    env = env.receive(sessChan2, "Bob", intT)
    env = env.leaveChoiceReceiveBranch

    env = env.leaveChoiceReceiveBlock
    env = env.leaveSessionMethod

    checkInferred(env, fooMethod, sessChan, "X1", List(
      createInteraction(null, bobRole, stringTRef),
      createChoice(bobRole, null, emptyBody(List(stringTRef, intTRef)))
    ))
    checkInferred(env, fooMethod, sessChan2, "X1", List (
      createInteraction(aliceRole, null, intTRef),
      createInteraction(bobRole, null, intTRef)
    ))
  }

  test("method inference, interleaved sessions, choice branches, uneven interleaved session") {
    var env = sessionMethod(fooMethod, sessChan, sessChan2)
    
    env = env.enterChoiceReceiveBlock(sessChan, "Bob")

    env = env.enterChoiceReceiveBranch(stringT)
    env = env.receive(sessChan2, "Bob", intT)
    env = env.leaveChoiceReceiveBranch

    env = env.enterChoiceReceiveBranch(intT)
    env = env.receive(sessChan2, "Bob", intT)
    env = env.receive(sessChan2, "Bob", intT)

    intercept[SessionTypeCheckingException] {
      env = env.leaveChoiceReceiveBranch
      env = env.leaveChoiceReceiveBlock
    }
  }

  test("inferred method call, basic send/receive proto") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.send(sessChan, "Bob", stringT)
    env = env.leaveSessionMethod
    
    env = new JoinBlockTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, sendStringModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.delegation(fooMethod, List(sessChan))
    env = env.leaveJoin
  }

  test("inferred method call, choice proto, receive side") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(stringT)
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(intT)
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    env = env.leaveSessionMethod

    env = new JoinBlockTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, choiceProtoModel)
    env = env.enterJoin(sharedChan, "Bob", sessChan)
    env = env.delegation(fooMethod, List(sessChan))
    env = env.leaveJoin
  }

  test("inferred method call, choice proto, wrong role: error") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.enterChoiceReceiveBlock(sessChan, "Foo")
    env = env.enterChoiceReceiveBranch(stringT)
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(intT)
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    env = env.leaveSessionMethod

    env = new JoinBlockTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, choiceProtoModel)
    env = env.enterJoin(sharedChan, "Bob", sessChan)
    intercept[SessionTypeCheckingException] {
      env = env.delegation(fooMethod, List(sessChan))
    }
  }

  test("inferred method call, choice proto, wrong body of when branch: error") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(stringT)
    env = env.send(sessChan, "Bar", intT) // not in protocol: error
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(intT)
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    env = env.leaveSessionMethod

    env = new JoinBlockTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, choiceProtoModel)
    env = env.enterJoin(sharedChan, "Bob", sessChan)
    intercept[SessionTypeCheckingException] {
      env = env.delegation(fooMethod, List(sessChan))
    }
  }

  test("inferred method call, choice proto, send side") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.enterThen
    env = env.send(sessChan, "Bob", stringT)
    env = env.enterElse
    env = env.send(sessChan, "Bob", intT)
    env = env.leaveIf
    env = env.leaveSessionMethod

    env = new JoinBlockTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, choiceProtoModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.delegation(fooMethod, List(sessChan))
    env = env.leaveJoin
  }

  test("inferred method call, choice proto, send side, wrong label: error") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.enterThen
    env = env.send(sessChan, "Bob", objectT) // should be stringT -> error
    env = env.enterElse
    env = env.send(sessChan, "Bob", intT)
    env = env.leaveIf
    env = env.leaveSessionMethod

    env = new JoinBlockTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, choiceProtoModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    intercept[SessionTypeCheckingException] {
      env = env.delegation(fooMethod, List(sessChan))
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
    env = env.send(sessChan, "Bob", stringT)
    env = env.delegation(fooMethod, List(sessChan))
    env = env.leaveSessionMethod
    
    env = new JoinBlockTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, recurModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.delegation(fooMethod, List(sessChan))
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
    //env = env.send(sessChan, "Bob", intT)
    env = env.delegation(ymethod, List(sessChan))
    env = env.leaveSessionMethod
    assert(notEmpty(env.asInstanceOf[InferredTypeRegistry].inferredSessionType(xmethod, sessChan)))
    println("after assert")

    env = env.enterSessionMethod(ymethod, List(sessChan))
    env = env.enterThen
    env = env.send(sessChan, "Bob", intT)
    env = env.delegation(xmethod, List(sessChan))
    env = env.enterElse
    env = env.send(sessChan, "Bob", stringT)
    env = env.delegation(ymethod, List(sessChan))
    env = env.leaveIf
    env = env.leaveSessionMethod

    assert(notEmpty(env.asInstanceOf[InferredTypeRegistry].inferredSessionType(xmethod, sessChan)))
    assert(notEmpty(env.asInstanceOf[InferredTypeRegistry].inferredSessionType(ymethod, sessChan)))    

    env = new JoinBlockTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, multiRecurModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.delegation(xmethod, List(sessChan))
    env = env.leaveJoin
  }
  	
  ignore("scoping of inferred methods") {
    // will require keeping even the join block visitor informed of method definitions,
    // so that it can bring previously inferred methods in and out of scope
    // or maybe the Symbol objects already have enough info?
  }

  test("linearity: cannot use session channel after passing it to a method") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.leaveSessionMethod

    env = new JoinBlockTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, sendStringModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.delegation(fooMethod, List(sessChan))
    intercept[SessionTypeCheckingException] {
      env = env.send(sessChan, "Bob", stringT) // reuse of sessChan not allowed
    }
  }
}
