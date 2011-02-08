package uk.ac.ic.doc.sessionscala.compiler

import org.scalatest.{FunSuite, Tag}
import org.scribble.protocol.model._
import org.scalatest.matchers.ShouldMatchers

/**
 * Created by: omp08
 */

class TypecheckingTests extends FunSuite with SessionTypingEnvironments
                                         with ScalaCompilerSetup
                                         with ScribbleParsing
                                         with ShouldMatchers
                                         with EnvironmentsFixtures {

  import global._

  test("top-level enter join, unregistered channel: error") {
    intercept[SessionTypeCheckingException] {
      topEnv.enterJoin(sharedChan, "A", sessChan)
    }
  }

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

  test("closure env don't lose inferred method types") {
    var env: SessionTypingEnvironment = new MethodSessionTypeInferenceTopLevelEnv
    env = env.enterClosure(Nil)
    env = env.enterSessionMethod(fooMethod, List(sessChan))
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.leaveSessionMethod(Nil)
    env = env.leaveClosure

    env = new ProcessBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, sendStringModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.delegation(fooMethod, List(sessChan), Nil)
    env = env.leaveJoin
  }

  test("branch receive with more branches than specified, valid through subtyping") {
    var env = join(choiceProtoModel, "Bob")
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(sig(stringT))
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(sig(intT))
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(sig("quit"))
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    env = env.leaveJoin
  }

  test("branch receive with more branches than specified, but extra branch has overlapping msigs") {
    var env = join(choiceProtoModel, "Bob")
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
      env = env.enterChoiceReceiveBranch(sig(stringT))
      env = env.leaveChoiceReceiveBranch
      env = env.enterChoiceReceiveBranch(sig(intT))
      env = env.leaveChoiceReceiveBranch
      env = env.enterChoiceReceiveBranch(sig("quit"))

        env = env.enterChoiceReceiveBlock(sessChan, "Foo")
          env = env.enterChoiceReceiveBranch(sig(anyT))
          env = env.leaveChoiceReceiveBranch
          env = env.enterChoiceReceiveBranch(sig(stringT))
          env = env.leaveChoiceReceiveBranch
      intercept[SessionTypeCheckingException] {
        env = env.leaveChoiceReceiveBlock
      }
  }

  test("branch receive with more branches than specified, but extra branch has overlapping msigs with labels") {
    var env = join(choiceProtoModel, "Bob")
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
      env = env.enterChoiceReceiveBranch(sig(stringT))
      env = env.leaveChoiceReceiveBranch
      env = env.enterChoiceReceiveBranch(sig(intT))
      env = env.leaveChoiceReceiveBranch
      env = env.enterChoiceReceiveBranch(sig("extra"))

        env = env.enterChoiceReceiveBlock(sessChan, "Foo")
          env = env.enterChoiceReceiveBranch(sig("label", anyT))
          env = env.leaveChoiceReceiveBranch
          env = env.enterChoiceReceiveBranch(sig("label", stringT))
          env = env.leaveChoiceReceiveBranch
      intercept[SessionTypeCheckingException] {
        env = env.leaveChoiceReceiveBlock
      }
  }

  test("inferred branch receive with more branches than specified at point of use") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(sig(stringT))
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(sig(intT))
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(sig("extra"))
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    env = env.leaveSessionMethod(Nil)

    env = new ProcessBlocksPassTopLevelEnv(env.asInstanceOf[InferredTypeRegistry])
    env = env.registerSharedChannel(sharedChan, choiceProtoModel)
    env = env.enterJoin(sharedChan, "Bob", sessChan)
    env = env.delegation(fooMethod, List(sessChan), Nil)
    env = env.leaveJoin
  }

  test("can invite listed participants") {
    var env = topEnv.registerSharedChannel(sharedChan, sendStringModel)
    env = env.invite(sharedChan, List("Bob", "Alice"))
  }

  test("cannot invite for other roles") {
    var env = topEnv.registerSharedChannel(sharedChan, sendStringModel)
    intercept[SessionTypeCheckingException] {
      env = env.invite(sharedChan, List("Bob", "Foo"))
    }
  }

  test("cannot invite twice for the same role") {
    var env = topEnv.registerSharedChannel(sharedChan, sendStringModel)
    intercept[SessionTypeCheckingException] {
      env = env.invite(sharedChan, List("Bob", "Bob"))
    }
  }

  test("cannot invite twice for the same role in separate calls to invite") {
    var env = topEnv.registerSharedChannel(sharedChan, sendStringModel)
    env = env.invite(sharedChan, List("Bob", "Alice"))
    intercept[SessionTypeCheckingException] {
      env = env.invite(sharedChan, List("Bob"))
    }
  }

  test("can invite listed participants in several calls to invite") {
    var env = topEnv.registerSharedChannel(sharedChan, sendStringModel)
    env = env.invite(sharedChan, List("Bob"))
    env = env.invite(sharedChan, List("Alice"))
  }

  test("entering closure doesn't interfere with invites (happy path)", Tag("wip")) {
    var env = topEnv.enterClosure(Nil)
    env = env.registerSharedChannel(sharedChan, sendStringModel)
    env = env.invite(sharedChan, List("Bob"))
    env = env.invite(sharedChan, List("Alice"))
    env = env.leaveClosure
  }

  test("entering closure doesn't interfere with invites (error path)") {
    var env = topEnv.enterClosure(Nil)
    env = env.registerSharedChannel(sharedChan, sendStringModel)
    env = env.invite(sharedChan, List("Bob"))
    intercept[SessionTypeCheckingException] {
      env = env.invite(sharedChan, List("Foo"))
    }
  }

test("entering if doesn't interfere with invites (happy path)") {
    var env = topEnv.registerSharedChannel(sharedChan, sendStringModel)
    env = env.invite(sharedChan, List("Bob"))
    env = env.enterThen
    env = env.invite(sharedChan, List("Alice"))
    env = env.enterElse
    env = env.invite(sharedChan, List("Alice"))
    env = env.leaveIf
  }

  test("entering if doesn't interfere with invites (error path)") {
    var env = topEnv.registerSharedChannel(sharedChan, sendStringModel)
    env = env.invite(sharedChan, List("Bob"))
    env = env.enterThen
    intercept[SessionTypeCheckingException] {
      env = env.invite(sharedChan, List("Foo"))
    }
  }

  test("nested leaveJoin in closure") {
    var env = join(sendStringModel, "Alice")
    env = env.enterClosure(Nil)
    env = env.registerSharedChannel(sharedChan2, sendStringModel)
    env = env.enterJoin(sharedChan2, "Alice", sessChan2)
    env = env.send(sessChan2, "Bob", sig(stringT))
    env = env.leaveJoin
    env = env.leaveClosure
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.leaveJoin
  }

  test("send in if in closure") {
    var env = topEnv.registerSharedChannel(sharedChan, sendStringModel)
    env = env.enterClosure(Nil)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.enterThen
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.enterElse
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.leaveIf
    env = env.leaveJoin
    env = env.leaveClosure
  }

  test("closures forbid invites") {
    var env = topEnv.registerSharedChannel(sharedChan, sendStringModel)
    env = env.enterClosure(Nil)
    intercept[SessionTypeCheckingException] {
      env = env.invite(sharedChan, List("Alice"))
    }
  }
}
