package chapter6

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (num, newState) = rng.nextInt
    if (num < 0) (-(num + 1), newState)
    else (num, newState)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (num, newState) = rng.nextInt
    (num / (Int.MaxValue.toDouble + 1), newState)
  }

  def intDouble1(rng: RNG): ((Int,Double), RNG) = {
    val (int, newState) = rng.nextInt
    val (dou, newState2) = double(newState)
    ((int, dou), newState2)
  }

  def intDouble2(rng: RNG): ((Int,Double), RNG) = {
    rng.nextInt match {
      case (int, s2) =>
        val (dou, s3) = double(s2)
        ((int, dou), s3)
    }
  }

  def doubleIntBp(rng: RNG): ((Double,Int), RNG) = {
    val ((int, dou), s2) = intDouble1(rng)
    ((dou, int), s2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def intsRecurs(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def goInts(collector: List[Int], current: RNG, countLeft: Int) : (List[Int], RNG) = {
      if (countLeft > 0) {
        val (newInt, newRNG) = current.nextInt
        goInts(newInt :: collector, newRNG, countLeft - 1)
      } else (collector, current)
    }

    goInts(List(), rng, count)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    List.tabulate(count)(c => {
      val (newInt, newRNG) = rng.nextInt
      ???
    })
    ???
  }
}
