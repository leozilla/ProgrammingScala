package chapter8

import chapter6._

case class Gen[A](sample: State[RNG,A]) {

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](bGen: Gen[B])(f: (A, B) => C): Gen[C] =
    bGen flatMap(b => Gen(sample.map(a => f(a, b))))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(sizeGen: Gen[Int]): Gen[List[A]] =
    sizeGen flatMap (size => listOfN(size))

  def apply(rng: RNG) = sample(rng)

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))
  def listOfN(listLength: Int): Gen[List[A]] = {
    val listOfStates = List.fill(listLength)(sample)
    Gen(State.sequence(listOfStates))
  }
}

object Gen {

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g listOfN n)

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g listOfN (n max 1))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    ???
    // Gen(State(RNG.nonNegativeLessThan(stopExclusive - start).map(_ + start)))
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



  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean flatMap (takeLeft => if (takeLeft) g1 else g2)



  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] =
    double flatMap (weight => if (g1._2 <= weight) g1._1 else g2._1)

  def weightedBuch[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d =>
      if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  def weighted2[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    double.flatMap(d => if (d < g1Threshold) g1._1 else g2._1)
  }

  /*
  /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
   */
}