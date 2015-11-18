package chapter8

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}

object ListSpecification extends Properties("List") {

  property("sum holds on reverse") = forAll { (l: List[Int]) =>
    ListToTest.sum(l) == ListToTest.sum(l.reverse)
  }

  property("sum of list with same items") = forAll { (l: List[Int]) =>
    (l.toSet.size == 1) ==> (ListToTest.sum(l) == l.head * l.size)
  }

  property("max holds on reverse") = forAll { (l: List[Int]) =>
    (l.size == 1) ==> (l.max == l.reverse.max)
  }
}
