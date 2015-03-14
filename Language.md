# Language features #

This page summarizes the features added by this Scala extension.

## Shared channels ##

Shared channels are special objects associated with a Scribble protocol. Their purpose is the initiation of multiparty sessions. They offer two different session initiation schemes: `join`, and `invite`/`accept`.

Currently, session-scala offers two implementations of shared channels: local channels (shared memory communication), and AMQP channels.

Both methods creating shared channels take a String parameter. This can either be the name of a Scribble file, or an inline Scribble protocol definition.

```
withLocalChannel("protocol.spr") { sharedChannel =>
  ...
}

withAMQPChannel("protocol.spr") { sharedChannel =>
  ...
}
```

By default, the AMQP shared channel connects to a broker running on localhost, port 5672, with the standard guest credentials. These can all be set to different values.

Shared channels cannot be assigned to another var/val, nor passed as parameters of a function call.


---


## Session initiation ##

Once you've created a shared channel, you'll want to start a session over it.
Starting a session requires all participating processes to call a method on the shared channel, giving the role they expect to play in the session as parameter.

Roles in the session-scala API are always Symbol literals (unique strings), for which the Scala notation is `'Foo`.

### With `join` ###

This initiation scheme has the shared channel behave like a barrier: all threads/actors calling `join` block until all roles are present, then one of each role is unblocked.

```
withLocalChannel("""
  protocol Test { role Alice, Bob; String from Alice to Bob; }
""") { sharedChannel =>
  
  actor { sharedChannel.join('Alice) { s =>
    s('Bob) ! "test message"
  }}
  
  sharedChannel.join('Bob) { s =>
    s('Alice).?[String]
  }
}
```

Currently this is only implemented in the local shared channel.

### With `invite` and `accept` ###

This is a more flexible scheme. One process invites other processes to join a session as a given role.
The other processes are blocked, waiting for an invite for the role they're prepared to play.
As soon as a process is invited, it can start communicating. If another role has yet to be assigned to a process, messages for this role will be queued and delivered later.

On host A:
```
withAMQPChannel("""
  protocol Test { role Alice, Bob; String from Alice to Bob; }
""") { sharedChannel =>
  
  sharedChannel.invite('Alice -> localhost, 'Bob -> "hostB.domain.name"
  sharedChannel.accept('Alice) { s =>
    s('Bob) ! "test message"
  }
}
```

On host B:
```
withAMQPChannel("""
  protocol Test { role Alice, Bob; String from Alice to Bob; }
""") { sharedChannel =>
  
  sharedChannel.accept('Bob) { s =>
    s('Alice).?[String]
  }
}
```

Currently this is only implemented in the AMQP shared channel.

The compiler plugin statically checks that roles are invited only once, each.
There is also a `forwardInvite` method, which first blocks waiting for the given invites to arrive, then forwards them to another process.


---


## Session communication ##

Both `join` and `accept` take a one-argument closure. The argument (`s` in the examples) is the session channel. The compiler plugin tracks its use and verifies that the code implements the Scribble protocol faithfully.

### Interactions ###
The most basic construct in Scribble: a message sent from role _`A`_ to role _`B`_.
In Scribble syntax:
  * `String from A to B`
  * `mylabel(Int) from A to B`

To implement it:
  * Sending:
    * `s('B) ! "message"` or
    * `s('B) ! ('mylabel, 42)`
  * Receiving:
    * `s('A).?[String]` or
    * `val ('mylabel, v: Int) = s('A).??`

### Choice ###
In Scribble syntax:
```
choice from A to B {
  Int: ...
  mylabel(String): ...
}
```

To implement it:
  * Sending: (identical to Interactions!)
    * `s('B) ! 42`
    * `s('B) ! ('mylabel, "msg")`
  * Receiving:
```
s('A).receive {
  case i: Int => ...
  case ('label, s: String) => ...
}
```

### Recursion ###
```
rec X {
  String from A to B;
  choice from B to A {
    continue(): X;
    stop():
  }
}
```

```
sharedChan.join('A) { s =>
  def recurse(s: SessionChannel) {
    s('B) ! "foo"
    s('B).receive {
      case 'continue => recurse(s)
      case 'stop => 
    }
  }
  recurse(s)
}
```

```
def recB(s: SessionChannel) {
  val str = s('A).?[String]
  if (str == "foo") s('A) ! 'stop
  else s('A) ! 'continue
}
sharedChan.join('B) { s =>
  recB(s)
}
```