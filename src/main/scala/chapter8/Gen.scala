package chapter8

import chapter6._

case class Gen[A](sample: State[RNG,A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(sizeGen: Gen[Int]): Gen[List[A]] =
    sizeGen flatMap (size => Gen.listOfN(size, this))

}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    // val runFunc: RNG => (Int, RNG) = RNG.between(RNG.int)(start, stopExclusive)
    val state: State[RNG, Int] = State(RNG.nonNegativeInt).map(x => x)
    Gen(state)
  }

  /*
  def chooseTuple(start: Int, stopExclusive: Int): Gen[(Int, Int)] = {
      val left = choose(start, stopExclusive)
      val right = choose(start, stopExclusive)
      (left, right)
  }*/

  def unit[A](a: => A): Gen[A] =
    Gen(State(RNG.unit(a)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.int).map(_ >= 0))

  def double: Gen[Double] =
    Gen(State(RNG.double))

  def listOfN[A](listLength: Int, itemGenerator: Gen[A]): Gen[List[A]] = {
    val listOfStates = List.fill(listLength)(itemGenerator.sample)
    Gen(State.sequence(listOfStates))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean flatMap (takeLeft => if (takeLeft) g1 else g2)

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] =
    Gen.double flatMap (weight => if (g1._2 <= weight) g1._1 else g2._1)

  /*
  /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
   */
}