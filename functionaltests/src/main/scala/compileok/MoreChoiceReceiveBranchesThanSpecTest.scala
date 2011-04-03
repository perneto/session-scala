package compileok

import uk.ac.ic.doc.sessionscala.{PublicPort}

object MoreChoiceReceiveBranchesThanSpecTest {
  def main(args: Array[String]) {
    val sharedChannel = PublicPort.newLocalPort("""
    protocol Branches {
      role Alice, Bob;
      choice from Bob to Alice {
        Int:
        String:
      }
    }
    """, 'Alice)
    sharedChannel.bind { s =>
      s.receive('Bob) {
        case s: String =>
        case i: Int =>
        case d: Double =>
      }
    }
  }
}