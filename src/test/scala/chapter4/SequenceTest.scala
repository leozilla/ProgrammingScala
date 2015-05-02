package chapter4

class SequenceTest extends UnitSpec {
  "Empty sequence" should "variance is None" in {
    val actual = Sequence.variance(Seq())
    actual should be (None)
  }

  "Sequence with one element" should "variance" in {
    val actual = Sequence.variance(Seq(1))
    actual should be (Some(0.0))
  }

  "Sequence with multiple elements1" should "variance" in {
    val actual = Sequence.variance(Seq(1,2,3,4,5))
    actual should be (Some(2.0))
  }

  "Sequence with multiple elements2" should "variance" in {
    val actual = Sequence.variance(Seq(1,1,2,3,5,8))
    actual should be (Some(6.222222222222221))
  }
}
