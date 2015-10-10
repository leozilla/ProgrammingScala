package chapter6

import org.scalatest._

class CandyDispenserSpec extends FlatSpec with Matchers {

  "No inputs" should "get back same machine" in {
    // given
    val machine = Machine(locked = true, 5, 10)

    // when
    val actual: State[Machine, (Int, Int)] = CandyDispenser.simulateMachine(List())

    // then
    actual.run(machine) should be ((5, 10), machine)
  }

  "Turn locked machine" should "not change state" in {
    // given
    val machine = Machine(locked = true, 5, 10)

    // when
    val actual: State[Machine, (Int, Int)] = CandyDispenser.simulateMachine(List(Turn))

    // then
    actual.run(machine) should be ((5, 10), machine)
  }

  "Insert coin into locked machine without candies" should "not change state" in {
    // given
    val machine = Machine(locked = true, 0, 10)

    // when
    val actual: State[Machine, (Int, Int)] = CandyDispenser.simulateMachine(List(Coin))

    // then
    actual.run(machine) should be ((0, 11), Machine(locked = false, 0, 11))
  }

  "Insert coin into locked machine with at least one candy" should "shall unlock machine" in {
    // given
    val machine = Machine(locked = true, 1, 10)

    // when
    val actual: State[Machine, (Int, Int)] = CandyDispenser.simulateMachine(List(Coin))

    // then
    actual.run(machine) should be ((0, 11), Machine(locked = false, 0, 11))
  }

  "Turn unlocked machine with at least one candy" should "shall unlock machine and dispense candy" in {
    // given
    val machine = Machine(locked = false, 1, 10)

    // when
    val actual: State[Machine, (Int, Int)] = CandyDispenser.simulateMachine(List(Turn))

    // then
    actual.run(machine) should be ((0, 10), Machine(locked = false, 0, 10))
  }
}