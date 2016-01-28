package chapter8

import java.util.concurrent.Executors

import chapter7.Par
import chapter7.Par.Par
import chapter8.Prop.forAll
import chapter8.SGen.listOf
import chapter8.SGen.listOf1
import org.scalatest.{Matchers, FunSuite}

class FuncTestingSpec extends FunSuite with Matchers {

  test("list max property test with empty list") {
    val smallInt = Gen.choose(-10,10)
    val maxProp = forAll(listOf(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    Prop.run(maxProp)
  }

  test("list max property test without empty list") {
    val smallInt = Gen.choose(-10,10)
    val maxProp = forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    Prop.run(maxProp)
  }

  test("sort list") {
    val ints = Gen.choose(-10,10)
    val minProp = forAll(listOf1(ints)) { ns =>
      val sorted = ns.sorted
      sorted.head == ns.min
    }
    val maxProp = forAll(listOf1(ints)) { ns =>
      val sorted = ns.sorted
      sorted.reverse.head == ns.max
    }

    Prop.run(minProp && maxProp)
  }

  val ES = Executors.newCachedThreadPool()

  test("par unit equal to map unit") {
    val prop = Prop.check {
      val p1 = Par.map(Par.unit(1))(_ + 1)
      val p2 = Par.unit(2)
      p1(ES).get == p2(ES).get
    }

    Prop.run(prop)
  }

  val S = Gen.weighted(
    Gen.choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, aa) => f(aa)(s).get }
}
