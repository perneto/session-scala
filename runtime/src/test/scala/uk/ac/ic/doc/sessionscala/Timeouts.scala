package uk.ac.ic.doc.sessionscala

import scala.actors.{Future, Futures, TIMEOUT}, Futures.future
import org.scalatest.FunSuite

/**
 * Created by: omp08
 */

trait Timeouts extends FunSuite {
  abstract class CallResult[+T]
  case class Ok[T](value: T) extends CallResult[T]
  case object Timeout extends CallResult[Nothing]
  case class Except(e: Throwable) extends CallResult[Nothing]

  def callWithTimeout[T](timeout: Int)(block: => T): CallResult[T] = {
    val f: Future[CallResult[T]] = future {
      try {
        Ok(block)
      } catch {
        case e => Except(e)
      }
    }
    f.inputChannel.receiveWithin(timeout) {
      case x:CallResult[T] => x
      case TIMEOUT => Timeout
    }
  }
  def withTimeout[T](timeout: Int)(block: => T): T = {
    callWithTimeout(timeout)(block) match {
      case Ok(v) => v
      case Except(e) => throw e
      case Timeout => fail("Timeout (exceeded " + timeout + "ms)")
    }
  }

  def withTimeoutAndWait[T](timeout: Int, wait: Int)(block: => T): T = {
    val ret = withTimeout(timeout)(block)
    Thread.sleep(wait)
    ret
  }

  def withTimeoutAndWait[T](block: => T): T = withTimeoutAndWait(5000,300)(block)
  def sleep() = Thread.sleep(250)
  def sleep(millis: Int) = Thread.sleep(millis)
  
  def expectTimeout(timeout: Int)(block: => Any): Unit = 
    callWithTimeout(timeout)(block) match {
      case Ok(v) => fail("Should have timed out, but returned normally. Return value: " + v)
      case Timeout =>
      case Except(e) => throw e
    }
}

