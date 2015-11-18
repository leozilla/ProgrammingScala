package chapter8

import org.scalatest.{Matchers, FunSuite}

/**
  * Created by dleonhar on 18.11.2015.
  */
class Gen$Test extends FunSuite with Matchers {

  test("bla") {
    val gen = Gen.choose(1, 2)
  }
}
