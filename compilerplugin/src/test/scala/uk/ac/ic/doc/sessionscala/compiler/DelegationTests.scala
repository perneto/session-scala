package uk.ac.ic.doc.sessionscala.compiler

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class DelegationTests extends FunSuite with SessionTypingEnvironments
                                      with ScalaCompilerSetup
                                      with ScribbleParsing
                                      with ShouldMatchers
                                      with EnvironmentsFixtures {
  import global.newTermName

  test("inferred method call, basic send/receive proto") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.leaveSessionMethod(Nil)

    env = new ProcessBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerAddress(sharedChan, sendStringModel, "Alice")
    env = env.enterJoin(sharedChan, sessChan)
    env = env.delegation(fooMethod, List(sessChan), Nil)
    env = env.leaveJoin
  }

  test("inferred method call, choice proto, receive side") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.enterChoiceReceiveBlock(sessChan, Some("Alice"))
    env = env.enterChoiceReceiveBranch(None, sig(stringT))
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(None, sig(intT))
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    env = env.leaveSessionMethod(Nil)

    env = new ProcessBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerAddress(sharedChan, choiceProtoModel, "Bob")
    env = env.enterJoin(sharedChan, sessChan)
    env = env.delegation(fooMethod, List(sessChan), Nil)
    env = env.leaveJoin
  }

  // Not checking anymore in preparation for generalized choice
  ignore("inferred method call, choice proto, wrong role: error") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.enterChoiceReceiveBlock(sessChan, Some("Foo"))
    env = env.enterChoiceReceiveBranch(None, sig(stringT))
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(None, sig(intT))
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    env = env.leaveSessionMethod(Nil)

    env = new ProcessBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = join(env, choiceProtoModel, "Bob")
    intercept[SessionTypeCheckingException] {
      env = env.delegation(fooMethod, List(sessChan), Nil)
    }
  }

  test("inferred method call, choice proto, wrong body of when branch: error") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.enterChoiceReceiveBlock(sessChan, Some("Alice"))
    env = env.enterChoiceReceiveBranch(None, sig(stringT))
    env = env.send(sessChan, "Bar", sig(intT)) // not in protocol: error
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(None, sig(intT))
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    env = env.leaveSessionMethod(Nil)

    env = new ProcessBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = join(env, choiceProtoModel, "Bob")
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

    env = new ProcessBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = join(env, choiceProtoModel, "Alice")
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

    env = new ProcessBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = join(env, choiceProtoModel, "Alice")
    intercept[SessionTypeCheckingException] {
      env = env.delegation(fooMethod, List(sessChan), Nil)
    }
  }

  test("inferred, recursive method call - unroll recursion first") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.delegation(fooMethod, List(sessChan), Nil)
    env = env.leaveSessionMethod(Nil)

    env = new ProcessBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = join(env, recurModel, "Alice")
    env = env.delegation(fooMethod, List(sessChan), Nil)
    env = env.leaveJoin
  }

  test("inferred, recursive method call, multiple recursion labels") {
    var env = sessionMethod(xmethod, sessChan)
    //env = env.send(sessChan, "Bob", sig(intT))
    env = env.delegation(ymethod, List(sessChan), Nil)
    env = env.leaveSessionMethod(Nil)
    assert(notEmpty(env.asInstanceOf[InferredTypeRegistry].inferredSessionType(xmethod, 0)))

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

    env = new ProcessBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = join(env, multiRecurModel, "Alice")
    env = env.delegation(xmethod, List(sessChan), Nil)
    env = env.leaveJoin
  }

  test("linearity: cannot use session channel after passing it to a method") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.leaveSessionMethod(Nil)

    env = new ProcessBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = join(env, sendStringModel, "Alice")
    env = env.delegation(fooMethod, List(sessChan), Nil)
    intercept[SessionTypeCheckingException] {
      env = env.send(sessChan, "Bob", sig(stringT)) // reuse of sessChan not allowed
    }
  }

  test("returning a channel from session method") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.leaveSessionMethod(List(sessChan))

    env = new ProcessBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = join(env, sendStringModel, "Alice")
    val returnedSessChan = newTermName("returnedChan")
    env = env.delegation(fooMethod, List(sessChan), List(returnedSessChan))
    env = env.send(returnedSessChan, "Bob", sig(stringT))
    env = env.leaveJoin
  }

  test("inverted channel order when returning channels from session method") {
    val retChan = newTermName("returnedChan")
    val retChan2 = newTermName("returnedChan2")

    var env = sessionMethod(fooMethod, sessChan, sessChan2)
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.leaveSessionMethod(List(sessChan2, sessChan))

    env = new ProcessBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerAddress(sharedChan, twoMsgProto, "Alice")
    env = env.registerAddress(sharedChan2, twoMsgProto, "Alice")

    env = env.enterJoin(sharedChan, sessChan)
    env = env.enterJoin(sharedChan, sessChan2)

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

    env = new ProcessBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = join(env, sharedChan, sendStringModel, "Alice", sessChan2)
    env = env.delegation(fooMethod, List(sessChan2), Nil)
    env = env.leaveJoin
  }

  test("incomplete session on channel bound by session method return value") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.leaveSessionMethod(List(sessChan))

    env = new ProcessBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerAddress(sharedChan, twoMsgProto, "Alice")
    env = env.enterJoin(sharedChan, sessChan)
    env = env.delegation(fooMethod, List(sessChan), List(sessChan2))
    intercept[SessionTypeCheckingException] {
      // sessChan2 still has Int from Bob to Alice
      env = env.leaveJoin
    }
  }

  test("nested delegation, different parameter order") {
    var env = sessionMethod(xmethod, sessChan, sessChan2)
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.send(sessChan2, "Bob", sig(stringT))
    env = env.delegation(ymethod, List(sessChan2, sessChan), Nil)
    env = env.leaveSessionMethod(Nil)

    env = env.enterSessionMethod(ymethod, List(sessChan2, sessChan))
    env = env.receive(sessChan, "Bob", sig(intT))
    env = env.leaveSessionMethod(Nil)

    env = new ProcessBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerAddress(sharedChan, twoMsgProto, "Alice")
    env = env.registerAddress(sharedChan2, sendStringModel, "Alice")
    env = env.enterJoin(sharedChan, sessChan)
    env = env.enterJoin(sharedChan2, sessChan2)
    env = env.delegation(xmethod, List(sessChan, sessChan2), Nil)
    env = env.leaveJoin
  }

}