package chapter3

object ChapterThree {

  def main(args: Array[String]): Unit = {
    // exercise 3.1
    println(exercise3p1(List(1,2,3,4,5)))

    // exercise 3.2
    println(List.tail(List(1,2,3)))

    // exercise 3.3
    println(List.setHead(List(1,2,3), 4))
    // println(List.setHead(Nil, 1))

    // exercise 3.4 drop
    println("List.drop(List(1,2,3,4,5,6), 0) -> " + List.drop(List(1,2,3,4,5,6), 0))
    println("List.drop(List(1,2,3,4,5,6), 2) -> " + List.drop(List(1,2,3,4,5,6), 2))
    println("List.drop(List(1,2,3,4,5,6), 4) -> " + List.drop(List(1,2,3,4,5,6), 4))
    println("List.drop(List(1,2,3,4,5,6), 6) -> " + List.drop(List(1,2,3,4,5,6), 6))
    println("List.drop(List(1,2,3,4,5,6), 8) -> " + List.drop(List(1,2,3,4,5,6), 8))
  }

  def exercise3p1(is: List[Int]): Int = is match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
}