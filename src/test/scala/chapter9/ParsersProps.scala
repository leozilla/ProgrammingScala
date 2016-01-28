package chapter9

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object ParsersProps extends Properties("Parsers") {

  property("run") = forAll { (s: String) =>
    s == s
  }

  property("mapLaw") = forAll { (s: String) =>

  }
}
