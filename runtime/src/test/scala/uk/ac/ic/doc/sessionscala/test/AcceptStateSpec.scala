package uk.ac.ic.doc.sessionscala.test

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import uk.ac.ic.doc.sessionscala.AcceptState
import actors.Actor

class AcceptStateSpec extends FunSuite with ShouldMatchers {
  
  test("accept immediately with one participant") {
    val set = Set("Foo")
    val state = new AcceptState(set)
    val newState = state.received("Foo", null, null)
    assert(newState.roles == set)
    assert(newState.isComplete)
  }

  test("accept after both calls with 2 participants") {
    val set = Set("Foo", "Bar")
    val state = new AcceptState(set)
    val newState1 = state.received("Foo",null,null)
    assert(!newState1.isComplete, "not complete after just Foo")
    val newState2 = newState1.received("Bar",null,null)
    assert(newState2.roles == set, "roles should have both values")
    assert(newState2.isComplete, "should be complete")
  }

  test("creates one actor and replies") {
    val state = new AcceptState(Set("Foo"))
    var actorRan = false
    var proceedReceived = false
    val sender = Actor.actor {
      Actor.?
      proceedReceived = true
    }
    val newState = state.received("Foo", (s => actorRan = true), sender)
    newState.createActorsAndReply

    Thread.sleep(100)
    assert(actorRan, "actor should have started")
    assert(proceedReceived, "sender should have received reply")
  }


}