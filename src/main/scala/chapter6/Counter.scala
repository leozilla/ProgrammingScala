package chapter6

import chapter6.RNG
import chapter6.RNG.State

case class Counter(start: Int) {

  def inc() = Counter(start + 1)
}

object Actions {

  def sendEmail: State[Counter, String] =
    State(counter => ("sent email", counter.inc()))

  def printDocument: State[Counter, String] =
    State(counter => ("print document", counter.inc()))

  def main (args: Array[String]) {

    /*
    val startCounter = Counter(0)

    val stateTransitions = for {
      emailResult <- sendEmail
      printResult1 <- printDocument
      printResult2 <- printDocument
      _ <- State.modify(_ => Counter(1))
      c <- State.get
    } yield c

    print("count of events in system: " + stateTransitions.run(startCounter)._2)
    */
  }
}
