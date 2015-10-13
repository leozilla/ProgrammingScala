package chapter7

import java.util.concurrent.Executors

import chapter7.Par.Par
import org.scalatest._
import sun.security.provider.NativePRNG

import scala.util.Random

class ParTest extends FlatSpec with Matchers {

  "Unit" should "get back value immediately" in {
    // given
    val unit = Par.unit(1)

    // when
    val actualFuture = Par.run(Executors.newCachedThreadPool())(unit)

    // then
    actualFuture.get() should be (1)
  }

  "Forked computation" should "not be calculated in main thread" in {
    // given
    val testThreadId = Thread.currentThread().getId
    var forkedThreadId: Long = 0

    lazy val captureUnit = {
      forkedThreadId = Thread.currentThread().getId
      Par.unit(1) }
    val forked = Par.fork(captureUnit)

    // when
    val actualFuture = Par.run(Executors.newCachedThreadPool())(forked)

    // then
    actualFuture.get() should be (1)
    forkedThreadId should not be testThreadId
  }

  "Map2" should "combine values after computing them on a thread other than the main thread" in {
    // given
    val mapped = Par.map2(
        Par.fork(Par.lazyUnit(calcPiFor(1, 5550))),
        Par.fork(Par.lazyUnit(calcPiFor(1, 8050))))(_ + _)

    // when
    val actualFuture = Par.run(Executors.newFixedThreadPool(4))(mapped)

    // then
    actualFuture.get() should be (1)
  }

  def calcPiFor(start: Int, nrOfElements: Int) : Double = {
    var acc = 0.0
    for(i <- start until (start + Random.nextInt(nrOfElements)) ) acc += 4.0 * (1 - (i % 2) * 2) / (2 * i + 1)
    acc
  }
}
