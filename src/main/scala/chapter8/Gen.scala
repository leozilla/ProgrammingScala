package chapter8

import chapter6.{State, RNG}

case class Gen[A](sample: State[RNG,A]) {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = ???

  def unit[A](a: => A): Gen[A] = ???
}

object Gen {

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???
}