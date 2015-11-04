package chapter7

import java.util.concurrent.Executors

import chapter7.Nonblocking.NPar
import org.scalatest.concurrent.Timeouts
import org.scalatest.{Matchers, FlatSpec}

class NParTest  extends FlatSpec with Matchers with Timeouts {

  "Unit" should "get back value immediately" in {
    // given
    val unit = NPar.unit(1)

    // when
    val value = NPar.run(Executors.newCachedThreadPool())(unit)

    // then
    value should be (1)
  }
}
