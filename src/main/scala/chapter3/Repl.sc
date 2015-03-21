import chapter3._
import chapter3.List._

val digits = List(1,2,3,4,5)
val single = List(1)
val empty = Nil

val x = single match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + sum(t)
  case _ => 101
}