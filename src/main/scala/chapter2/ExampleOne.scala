package chapter2

/**
 * Created by DLeonhar on 01.03.2015.
 */
object ExampleOne extends App {

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(i: Int): Boolean =
      if (i <= 0) true
      else if (gt(as(i - 1), as(i))) loop(i - 1)
      else false

    loop(as.length - 1)
  }

  val sorted = isSorted(Array(0, 2, 3, 4, 123), (l: Int, r: Int) => l <= r)
  println(sorted)

  def fib(nth: Int): Int = {
    @annotation.tailrec
    def loop(curr: Int, prev: Int, acc: Int): Int =
      if (curr <= 0) acc
      else loop(curr - 1, acc, acc + prev)

    loop(nth, 1, 0)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    b => f(a, b)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  println(fib(1)) // 1
  println(fib(2)) // 1
  println(fib(3)) // 2
  println(fib(4)) // 3
  println(fib(5)) // 5
  println(fib(6)) // 8
  println(fib(7)) // 13
  println(fib(8)) // 21

  println("partial1")
  println(partial1[Int, Int, Int](10, (a, b) => a / b + 1)(2)) // 6
  println(partial1[Int, Int, Int](15, (a, b) => a / b + 2)(3)) // 7

  println("curry")
  println(curry[Int, Int, Int]((a, b) => (a / b) + 1)(10)(2)) // 6
  println(curry[Int, Int, Int]((a, b) => (a / b) + 2)(15)(3)) // 7

  println("compose")
  println(compose[Int, Int, Int](b => b / 5, a => a + 5)(10)) // 3
  println(compose[Int, Int, Int](b => b / 5, a => a + 10)(10)) // 4
}
