package chapter8

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}

object ListSpecification extends Properties("List") {

  property("sum holds on reverse") = forAll { (l: List[Int]) =>
    l.sum == l.reverse.sum
  }

  property("sum of list with same items") = forAll { (l: List[Int]) =>
    (l.toSet.size == 1) ==> (l.sum == l.head * l.size)
  }

  property("sum holds when summing list parts") = forAll { (l: List[Int]) =>
    val (left, right) = l.splitAt(l.size / 2)
    l.sum == left.sum + right.sum
  }

  property("max value should be greather or equal to all items") = forAll { (l: List[Int]) =>
    l.forall(i => i <= l.max)
  }
}
