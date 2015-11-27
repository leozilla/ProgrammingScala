package chapter8

import chapter5.Stream
import chapter6._
import chapter8.Prop.{MaxSize, TestCases, FailedCase, SuccessCount}

trait Prop {

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    def &&(p: Prop): Prop = Prop { (max, n, rng) => run.apply(max, n, rng) match {
        case Passed => p.run(max, n, rng)
        case falsified => falsified
      }
    }

    def ||(p: Prop): Prop = Prop { (max, n, rng) => run.apply(max, n, rng) match {
        case Passed => Passed
        case falsified => p.run(max, n, rng)
      }
    }
  }

  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???

  def forAll[A](aGen: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => randomStream(aGen)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      }
      catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize(1))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

object Prop {
  type Result = Option[(FailedCase, SuccessCount)]
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
}