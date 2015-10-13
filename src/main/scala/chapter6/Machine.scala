package chapter6

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyDispenser {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State(s => {
      val newMachine = inputs.foldRight(s)((i, acc) => transition(i, acc))
      ((newMachine.candies, newMachine.coins), newMachine)
    })
  }

  def transition(input: Input, m: Machine): Machine = ???
}