package buyerseller

case object OK
case object NotOK
case object Stop
case class Order(val price: Int)
case class Invoice(val price: Int)
case class Payment(val price: Int)

case class Price(value: Int)
