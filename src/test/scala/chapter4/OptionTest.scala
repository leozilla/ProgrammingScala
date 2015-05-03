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

  "Both Options of None" should "map2 to None" in {
    val actual = Option.map2(None, None)((a, b) => Some(1))
    actual should be (None)
  }

  "First Option of None" should "map2 to None" in {
    val actual = Option.map2(None, Some(1))((a, b) => b)
    actual should be (None)
  }

  "Second Option of None" should "map2 to None" in {
    val actual = Option.map2(Some(1), None)((a, b) => a)
    actual should be (None)
  }

  "Both Options of Some" should "map2 return applied function value" in {
    val actual =  Option.map2(Some(1), Some(2))(_ + _)
    actual should be (Some(3))
  }

  "Empty list" should "sequence is Some empty list" in {
    val actual = Option.sequence(List())
    actual should be (Some(List()))
  }

  "List with one Some" should "sequence is List with one element" in {
    val actual = Option.sequence(List(Some(1)))
    actual should be (Some(List(1)))
  }

  "List with one None" should "sequence is None" in {
    val actual = Option.sequence(List(None))
    actual should be (None)
  }

  "List with two Some values" should "sequence is List with two elements" in {
    val actual = Option.sequence(List(Some(1), Some(2)))
    actual should be (Some(List(1,2)))
  }

  "List with three values where one is None" should "sequence is None" in {
    val actual = Option.sequence(List(Some(1), Some(2), None))
    actual should be (None)
  }
}
