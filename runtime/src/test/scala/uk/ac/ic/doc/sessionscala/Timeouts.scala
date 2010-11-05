package uk.ac.ic.doc.sessionscala

import scala.actors.Actor, Actor._
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
    val a = actor {
      val ret = try {
        Ok(block)
      } catch {
        case e => Except(e)
      }
      react { case _ => Actor.reply(ret) }
    }
    (a !? (timeout, true)) match {
      case Some(x:CallResult[T]) => x 
      case None => Timeout
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
  def sleep() = Thread.sleep(300)
  
  def expectTimeout(timeout: Int)(block: => Any): Unit = 
    callWithTimeout(timeout)(block) match {
      case Ok(v) => fail("Should have timed out, but returned normally. Return value: " + v)
      case Timeout =>
      case Except(e) => throw e
    }
}

