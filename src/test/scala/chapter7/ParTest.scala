package chapter7

import java.util.concurrent.Executors

import chapter7.Par.Par
import org.scalatest._
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.{Seconds, Second, Millis, Span}
import sun.security.provider.NativePRNG

import scala.util.Random

class ParTest extends FlatSpec with Matchers with Timeouts {

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

  "parMap" should "map all values of a list after computing them on a thread other than the main thread" in {
    // given
    val mapped = Par.parMap(List(1,2,3,4,5,6,7,8,9,10))(x => x + 1)

    // when
    val actualFuture = Par.run(Executors.newFixedThreadPool(10))(mapped)

    // then
    actualFuture.get() should be (List(2,3,4,5,6,7,8,9,10,11))
  }

  "countWords" should "count number of words in parallel" in {
    // given
    val mapped = Par.parMap(List("one", "two words", "three words here"))(s => s.split(" "))
    val wordCount = Par.map(mapped)(words => words.length)

    // when
    val actualFuture = Par.run(Executors.newFixedThreadPool(10))(wordCount)

    // then
    actualFuture.get() should be (6)
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

  "fork" should "not deadlock when composing computations" in {
    // given
    val mapped = Par.fork(Par.fork(Par.unit(1)))

    // when
    val actualFuture = Par.run(Executors.newSingleThreadExecutor())(mapped)

    // then
    failAfter(Span(1, Second)) {
      actualFuture.get() should be (1)
    }
  }

  "sum raphael" should "see what happens" in {
    // given

    // when
    val actualFuture = Par.run(Executors.newCachedThreadPool())(sum(IndexedSeq(1,2,3,4)))

    // then
    failAfter(Span(1, Second)) {
      actualFuture.get() should be (10)
    }
  }

  "max of very big list" should "calculate slow when searching linear with one thread" in {
    // given
    val list = IndexedSeq.range(1, 10 * 1000 * 1000)

    // when
    val parMax = Par.run(Executors.newCachedThreadPool())(Par.max(list))

    // then
    failAfter(Span(3, Seconds)) {
      parMax.get() should be (24999999)
    }
  }

  "delay" should "not spawn new thread" in {
    // given
    val list = IndexedSeq.range(1, 1000)

    // when
    val parMax = Par.run(Executors.newCachedThreadPool())(Par.delay(Par.max(list)))

    // then
    failAfter(Span(3, Seconds)) {
      parMax.get() should be (999)
    }
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
//       Par.map2(Par.fork(sum(l)), sum(r))(_ + _)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  def calcPiFor(start: Int, nrOfElements: Int) : Double = {
    var acc = 0.0
    for(i <- start until (start + Random.nextInt(nrOfElements)) ) acc += 4.0 * (1 - (i % 2) * 2) / (2 * i + 1)
    acc
  }
}
