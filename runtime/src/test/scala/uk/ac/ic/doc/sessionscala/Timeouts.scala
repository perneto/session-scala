package uk.ac.ic.doc.sessionscala

import scala.actors.Actor, Actor._
import org.scalatest.FunSuite

/**
 * Created by: omp08
 */

trait Timeouts extends FunSuite {
  def withTimeout[T](timeout: Int)(block: => T): T = {
    val a = actor {
      val ret = block
      react { case _ => Actor.reply(ret) }
    }
    (a !? (timeout, true)) match {
      case Some(x:T) => x
      case None => fail("Timeout (exceeded " + timeout + "ms)")
    }
  }

  def withTimeoutAndWait[T](timeout: Int, wait: Int)(block: => T): T = {
    val ret = withTimeout(timeout)(block)
    Thread.sleep(wait)
    ret
  }

  def withTimeoutAndWait[T](block: => T): T = withTimeoutAndWait(5000,300)(block)
  def sleep() = Thread.sleep(300)
}