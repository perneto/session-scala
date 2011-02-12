package compileok

import uk.ac.ic.doc.sessionscala.{SharedChannel}

object MoreChoiceReceiveBranchesThanSpecTest {
  def main(args: Array[String]) {
    SharedChannel.withLocalChannel("""
    protocol Branches {
      role Alice, Bob;
      choice from Bob to Alice {
        Int:
        String:
      }
    }
    """) { sharedChannel =>

      sharedChannel.join('Alice) { s =>
        s('Bob).receive {
          case s: String =>
          case i: Int =>
          case d: Double =>
        }
      }
    }
  }
}