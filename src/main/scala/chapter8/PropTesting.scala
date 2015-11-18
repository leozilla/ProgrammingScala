package chapter8

object PropTesting {

  trait Gen[A] {

    def listOf(a: Gen[A]): Gen[List[A]]
  }
}
