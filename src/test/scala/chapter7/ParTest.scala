package chapter7

import chapter7.Par.Par
import org.scalatest._

class ParTest extends FlatSpec with Matchers {

  "Unit" should "get back value immediatly" in {
    // given
    val unitPar: Par[Int] = Par.unit(1)

    // when
    val actual: Int = Par.run(unitPar)

    // then
    actual should be (1)
  }

}
