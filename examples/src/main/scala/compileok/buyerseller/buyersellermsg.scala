package compileok.buyerseller

case object OK
case object NotOK
case object Stop
class Order(val price: Int)
class Invoice(val price: Int)
class Payment(val price: Int)

case class Price(value: Int)