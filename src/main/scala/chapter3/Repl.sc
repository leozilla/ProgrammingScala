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

def setHead2[A](as: List[A], h: A): List[A] =
  Cons(h, as)

def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  case Nil => Nil
  case Cons(h, t) =>
    if (f(h)) dropWhile(tail(l), f)
    else t
}

val in = List(1,2,3)
val actual = List.dropWhile(in, (x: Int) => x > 3)