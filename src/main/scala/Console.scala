import chapter6.{SimpleRNG, RNG}
import chapter6.RNG.{Turn, Coin, Machine, Rand}

object Console {

  def main(args: Array[String]) = {
    chapter6()
  }

  def chapter6(): Unit = {
    val state1 = SimpleRNG(System.nanoTime())
    val randify: Rand[Int] = RNG.unit(3)
    val mapped = RNG.map(randify)(a => a + 1)

    val (randInt, state2) = mapped(state1)
    println(randInt)

    println("RNG.doubleViaMap(state1)")
    println(RNG.doubleViaMap(state1))

    println("RNG.map2(randify, randify)((a, b) => a + b)(state1)")
    println(RNG.map2(randify, randify)((a, b) => a + b)(state1))

    val machine = Machine(locked = false, candies = 10, coins = 0)
    val shouldBeUnlocked = RNG.Machine.simulateMachine(List(Coin))
    println(shouldBeUnlocked.run(machine))

    val shouldBeLocked = RNG.Machine.simulateMachine(List(Coin, Turn))
    println(shouldBeLocked.run(machine))
  }
}
