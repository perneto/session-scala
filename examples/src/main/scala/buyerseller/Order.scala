package buyerseller

/**
 * Created by IntelliJ IDEA.
 * User: omp08
 * Date: Apr 6, 2010
 * Time: 5:52:41 PM
 * To change this template use File | Settings | File Templates.
 */

case object OK
case object NotOK
case object Stop
class Order(val price: Int)
class Invoice(val price: Int)
class Payment(val price: Int)

case class Price(value: Int)