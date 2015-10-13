package chapter7

import java.util.concurrent.Executors

import chapter7.Par.Par
import org.scalatest._

class ParTest extends FlatSpec with Matchers {

  "Unit" should "get back value immediately" in {
    // given
    val unit: Par[Int] = Par.unit(1)

    // when
    val actualFuture = Par.run(Executors.newCachedThreadPool())(unit)

    // then
    actualFuture.get() should be (1)
  }

  "Forked par" should "not be calculated in main thread" in {
    // given
    val testThreadId = Thread.currentThread().getId
    var forkedThreadId: Long = 0

    lazy val captureUnit = {
      forkedThreadId = Thread.currentThread().getId
      Par.unit(1) }
    val forked: Par[Int] = Par.fork(captureUnit)

    // when
    val actualFuture = Par.run(Executors.newCachedThreadPool())(forked)

    // then
    actualFuture.get() should be (1)
    forkedThreadId should not be testThreadId
  }
}
