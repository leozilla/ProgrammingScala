package chapter4

class OptionTest extends UnitSpec {
  "Option of Some" should "map to new value" in {
    val actual = Some(1).map(_.toString)
    actual should be (Some("1"))
  }

  "Option of None" should "map to None" in {
    val actual = None.map(_.toString)
    actual should be (None)
  }

  "Option of Some" should "getOrElse to existing value without evaluating default value" in {
    val actual = Some(1).getOrElse({ fail("default value was evaluated"); 99})
    actual should be (1)
  }

  "Option of None" should "getOrElse to lazy default value" in {
    val actual = None.getOrElse(99)
    actual should be (99)
  }

  "Option of Some" should "filter to same value if value satisfies filter predicate" in {
    val actual = Some(1).filter(_ >= 0)
    actual should be (Some(1))
  }

  "Option of Some" should "filter to None if value does not satisfy filter predicate" in {
    val actual = Some(1).filter(_ <= 0)
    actual should be (None)
  }

  "Option of None" should "filter to None if value satisfies filter predicate" in {
    val actual = None.filter(_ => true)
    actual should be (None)
  }

  "Option of None" should "filter to None if value does not satisfy filter predicate" in {
    val actual = None.filter(_ => false)
    actual should be (None)
  }

  "Option of Some" should "flatMap to new value if function does not fail" in {
    val actual = Some(1).flatMap(i => Some(i + 1))
    actual should be (Some(2))
  }

  "Option of Some" should "flatMap to None if function does fail" in {
    val actual = Some(1).flatMap(_ => None)
    actual should be (None)
  }

  "Option of None" should "flatMap to None" in {
    val actual = None.flatMap(i => Some(i))
    actual should be (None)
  }

  "Option of Some" should "orElse to this value without evaluating else value" in {
    val actual = Some(1).orElse({ fail("default value was evaluated"); Some(99)})
    actual should be (Some(1))
  }

  "Option of None" should "orElse to else value" in {
    val actual = None.orElse(Some(99))
    actual should be (Some(99))
  }
}
