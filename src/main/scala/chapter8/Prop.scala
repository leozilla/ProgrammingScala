package chapter8

import chapter8.Prop.{FailedCase, SuccessCount}

trait Prop {
  def &&(p: Prop): Prop = ???

  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

object Prop {

  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???
}