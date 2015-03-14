# Ideas #

The basic idea of session types is simple: we use a type to describe a communication protocol, which can be enforced by the compiler.
For this to be possible, communication needs to happen over an identified channel, and the compiler needs to forbid aliasing of the channel (otherwise typing becomes undecidable).

The typing system assumes asynchronous message passing communication.

Session types first appeared as binary session types (only two sides of a protocol). Multiparty session types extended the initial idea to describe protocols between several participants.

A multiparty session type definition contains roles definitions, which are bound to participants.

(work in progress)