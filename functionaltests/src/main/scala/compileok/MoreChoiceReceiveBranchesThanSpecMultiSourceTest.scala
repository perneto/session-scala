package compileok

import uk.ac.ic.doc.sessionscala.Address._

object MoreChoiceReceiveBranchesThanSpecMultiSourceTest {
  def main(args: Array[String]) {
    val sharedChannel = newLocalAddress("""
    protocol Branches {
      role Alice, Bob;
      choice from Bob to Alice {
        Int:
        String:
      }
    }
    """, 'Alice)
    sharedChannel.bind { s =>
      s.mreceive {
        case 'Bob -> (s: String) =>
        case 'Bob -> (i: Int) =>
        case 'Carol -> (d: Double) =>
      }
    }
  }
}