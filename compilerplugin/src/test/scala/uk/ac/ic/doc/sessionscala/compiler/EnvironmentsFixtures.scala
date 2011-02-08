package uk.ac.ic.doc.sessionscala.compiler

import org.scribble.protocol.model._
import org.scalatest.matchers.ShouldMatchers

trait EnvironmentsFixtures {
  self: ScalaCompilerSetup with SessionTypingEnvironments with ScribbleParsing with ShouldMatchers =>
  import global._

  val topEnv = new ProcessBlocksPassTopLevelEnv
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

  def inferred(env: SessionTypingEnvironment, method: Symbol, rank: Int): RecBlock =
    env.asInstanceOf[MethodSessionTypeInferenceTopLevelEnv]
                                    .inferredSessionType(method, rank)

  def checkInferred(env: SessionTypingEnvironment, meth: Symbol, chanRank: Int, label: String, block: List[Activity]) {
    val inf = inferred(env, meth, chanRank)
    val expected = createRecBlock(label, block)
    assert(inf === expected)
  }

  val emptyProtoModel = parse(
  """protocol Foo { role Alice; }
  """)

  val sendStringModel = parse(
  """protocol Foo {
       role Alice, Bob;
       String from Alice to Bob;
     }
  """)

  val choiceProtoModel = parse(
  """protocol Foo {
       role Alice, Bob;
       choice from Alice to Bob {
         String:
         Int:
       }
     }
  """)

  val twoMsgProto = parse(
  """protocol Foo {
       role Alice, Bob;
       String from Alice to Bob;
       Int from Bob to Alice;
     }
  """)

  val recurModel = parse(
  """protocol Foo {
       role Alice, Bob;
       rec X {
         String from Alice to Bob;
         X;
       }
     }
  """)

  val multiRecurModel = parse(
  """protocol Foo {
       role Alice, Bob;
       rec X {
         rec Y {
           choice from Alice to Bob {
             Int: X;
             String: Y;
           }
         }
       }
     }
  """)

  val xmethod = empty.newMethod(mkTermName("methodX"))
  val ymethod = empty.newMethod(mkTermName("methodY"))

}