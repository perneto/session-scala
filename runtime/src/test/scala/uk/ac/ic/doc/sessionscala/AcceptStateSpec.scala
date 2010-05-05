package uk.ac.ic.doc.sessionscala

import actors.Actor

class AcceptStateSpec extends Timeouts {

  test("not complete at first") {
    val state = new AcceptState(Set("Foo"))
    assert(!state.isComplete)
  }

  test("isComplete after one receive with one participant") {
    val set = Set("Foo")
    val state = new AcceptState(set)
    val newState = state.received("Foo", null, null)
    assert(newState.roles == set)
    assert(newState.isComplete)
  }

  test("isComplete after both calls with 2 participants") {
    val set = Set("Foo", "Bar")
    val state = new AcceptState(set)
    val newState1 = state.received("Foo",null,null)
    assert(!newState1.isComplete, "not complete after just Foo")
    val newState2 = newState1.received("Bar",null,null)
    assert(newState2.roles == set, "roles should have both values")
    assert(newState2.isComplete, "should be complete")
  }

  test("creates session channel and replies") {
    val state = new AcceptState(Set("Foo"))
    var sessionChanReceived = false
    val sender = Actor.actor {
      Actor.?
      sessionChanReceived = true
    }
    val newState = state.received("Foo", Actor.self, sender)
    val stateAfterCreate = newState.createSessionChanAndReply

    sleep
    assert(sessionChanReceived, "sender should have received reply")
    assert(!stateAfterCreate.isComplete, "state should not be complete anymore after actor creation")
  }

}