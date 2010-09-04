package uk.ac.ic.doc.sessionscala.compiler

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import tools.nsc.{Settings, Global}
import org.scribble.protocol.model._

/**
 * Created by: omp08
 */

class SessionTest extends FunSuite with SessionTypingEnvironments
        with ScribbleParsing
        with ScalaCompilerSetup
        with ShouldMatchers {
  
  val basicProtoModel = parse(
      """protocol Foo@Alice {
           role Bob;
           String to Bob;
         }
      """)

  val dualProtoModel = parse(
    """protocol Foo@Bob {
         role Alice;
         String from Alice;
       }
    """)

  val alice = new Role("Alice")
  val bob = new Role("Bob")

  test("interaction, send side") {
    var s = new Session(typeSystem, basicProtoModel)
    s = s.interaction(alice, bob, new TypeReference("String"))
    s.remaining should be ('empty)
    s should be ('complete)
  }

  test("interaction, receive side") {
    var s = new Session(typeSystem, dualProtoModel)
    s = s.interaction(alice, bob, new TypeReference("String"))
    s.remaining should be ('empty)
    s should be ('complete)
  }

  test("bad message type") {
    var s = new Session(typeSystem, dualProtoModel)
    intercept[SessionTypeCheckingException] {
      s = s.interaction(alice, bob, new TypeReference("Object"))
    }
  }

  val choiceProtoModel = parse(
  """protocol Foo@Alice {
       role Bob;
       choice to Bob {
        String {
          Int to Bob;
        }
        Int {}
       }
       Object to Bob;
     }  
  """)

  test("choice send side, complete") {
    var s = new Session(typeSystem, choiceProtoModel)
    s = s.interaction(alice, bob, new TypeReference("String"))
    s = s.interaction(alice, bob, new TypeReference("Int"))
    s = s.interaction(alice, bob, new TypeReference("Object"))
    s.remaining should be ('empty)
  }

  test("choice send side, bad label") {
    var s = new Session(typeSystem, choiceProtoModel)
    intercept[SessionTypeCheckingException] {
      s = s.interaction(alice, bob, new TypeReference("Object"))
    }
  }

  test("choice send side, missing interaction in branch") {
    var s = new Session(typeSystem, choiceProtoModel)
    s = s.interaction(alice, bob, new TypeReference("String"))
    intercept[SessionTypeCheckingException] {
      s = s.interaction(alice, bob, new TypeReference("Object"))
    }    
  }

  val choiceDual = parse(
    """protocol Foo@Bob {
         role Alice;
         choice from Alice {
          String {
            Int from Alice;
          }
          Int {}
         }
         Object from Alice;
       }  
    """)
  
  
  test("visitBranch") {
    var s = new Session(typeSystem, choiceDual)
    s = s.visitBranch(new MessageSignature(new TypeReference("String")), alice)
    s.remaining.size should be (1)
  }

}