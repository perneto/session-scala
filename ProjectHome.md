Welcome! session-scala is an extension of the [Scala](http://www.scala-lang.org/) programming language, containing:
  * A small library for parallel/distributed programming, with syntax similar to Scala Actors, currently supporting shared-memory and [AMQP](http://en.wikipedia.org/wiki/AMQP) communication.
  * A [compiler plugin](http://www.scala-lang.org/node/140) that checks processes against an associated multiparty session type, written using the [Scribble](http://scribble.org) specification language.

[Multiparty session types](MultipartySessionTypes.md) are global descriptions of a communication protocol between several participants.

Here's a simple session-scala example, with a two-participant session, a `Buyer` and a `Seller`:
```
import uk.ac.ic.doc.sessionscala.Address._
import actors.Actor._

object BuyerSellerExample {
  def main(args: Array[String]) {
    val buyer = newLocalAddress("buyerseller.spr", 'Buyer)
    val seller = newLocalAddress("buyerseller.spr", 'Seller)

    actor { startSession(buyer, seller) }
    actor {
      seller.bind { s =>
        val o = s.?[Order]('Buyer)
        s ! 'Buyer -> retrieveQuote(o)
        s.receive('Buyer) {
          case OK =>
            s ! 'Buyer -> new Invoice(2000)
            val payment = s.?[Payment]('Buyer)
          case NotOK =>
        }
      }
    }

    actor {
      buyer.bind { s =>
        s ! 'Seller -> new Order("Foo", 10)
        val price = s.?[Int]('Seller)
        if (price < 2000) {
          s ! 'Seller -> OK
          val invoice = s.?[Invoice]('Seller)
          s ! 'Seller -> createPayment(invoice, price)
        } else {
          s ! 'Seller -> NotOK
        }
      }
    }
  } 
}
```

The language extension is described further at [Language](Language.md); there's also more [Examples](Examples.md).

To compile source files using the plugin, use:
```
> scalac -cp runtime_2.8.1-0.1.jar -Xplugin:compilerplugin_2.8.1-0.1.jar myfile.scala ...
```

session-scala is developed in the [Theory of Computational Systems](http://www3.imperial.ac.uk/portal/page/portallive/computing/research/areas/THEORY/) group at [Imperial College London](http://www.imperial.ac.uk).