package uk.ac.ic.doc.sessionscala.compiler

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class InferenceTests extends FunSuite with SessionTypingEnvironments
                                      with ScalaCompilerSetup
                                      with ScribbleParsing
                                      with ShouldMatchers
                                      with EnvironmentsFixtures {
  import global.newTermName

  test("method inference, one send and receive") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.send(sessChan, "Bob", sig(stringT))
    env = env.receive(sessChan, "Bob", sig(stringT))
    env = env.leaveSessionMethod(Nil)

    // all inferred methods are inferred as potentially recursive,
    // and use a generated name as recursion variable.
    checkInferred(env, fooMethod, 0, "X1c0", List(
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

    checkInferred(env, fooMethod, 0, "X1c0", List(
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

    checkInferred(env, fooMethod, 0, "X1c0", List(
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

    checkInferred(env, fooMethod, 0, "X1c0", List(
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

    checkInferred(env, fooMethod, 0, "X1c0", List(
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

    checkInferred(env, fooMethod, 0, "X1c0", List(
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

    checkInferred(env, fooMethod, 0, "X1c0", List(
        createInteraction(null, aliceRole, charSequenceTRef), // send: infer supertype
        createInteraction(aliceRole, null, stringTRef) // receive: infer subtype
    ))
  }

  test("method inference, recursion") {
    var env = sessionMethod(fooMethod, sessChan)

    env = env.send(sessChan, "Alice", sig(stringT))
    env = env.delegation(fooMethod, List(sessChan), Nil)
    env = env.leaveSessionMethod(Nil)

    checkInferred(env, fooMethod, 0, "X1c0", List(
        createInteraction(null, aliceRole, stringTRef),
        createRecursion("X1c0")
    ))
  }

  test("method inference, interleaved sessions, basic send-receive") {
    var env = sessionMethod(fooMethod, sessChan, sessChan2)

    env = env.send(sessChan, "Alice", sig(stringT))
    env = env.receive(sessChan2, "Bob", sig(intT))
    env = env.leaveSessionMethod(Nil)

    checkInferred(env, fooMethod, 0, "X1c0", List (
      createInteraction(null, aliceRole, stringTRef)
    ))
    checkInferred(env, fooMethod, 1, "X1c1", List (
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

    checkInferred(env, fooMethod, 0, "X1c0", List (
      createChoice(null, aliceRole, emptyBody(List(intTRef, stringTRef)))
    ))
    checkInferred(env, fooMethod, 1, "X1c1", List (
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

    checkInferred(env, fooMethod, 0, "X1c0", List(
      createInteraction(null, bobRole, stringTRef),
      createChoice(bobRole, null, emptyBody(List(stringTRef, intTRef)))
    ))
    checkInferred(env, fooMethod, 1, "X1c1", List (
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

}