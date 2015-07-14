package chapter5

import org.scalatest._

class StreamTest extends FlatSpec with Matchers with OptionValues with Inside with Inspectors {

  "Empty sequence" should "variance is None" in {
    val take2 = Stream.apply(1, 2, 3).take(2)
    take2.toListRecursive should be (None)
  }
}
