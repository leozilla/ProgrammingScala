package chapter8

import chapter8.Prop.forAll
import chapter8.SGen.listOf
import org.scalatest.{Matchers, FunSuite}

class FuncTestingSpec extends FunSuite with Matchers {

  test("smallInt") {
    val smallInt = Gen.choose(-10,10)
    val maxProp = forAll(listOf(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    Prop.run(maxProp, maxSize = 5, testCases = 10)
  }
}
