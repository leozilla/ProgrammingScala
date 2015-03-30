package chapter3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // when passing Nil, MatchError will be generated
  def setHead[A](as: List[A], h: A): List[A] = as match {
    case Cons(_, t) => Cons(h, t)
  }

  def head[A](as: List[A]): A = as match {
    case Cons(h, _) => h
  }

  def size[A](as: List[A]): Int = as match {
    case Nil => 0
    case Cons(h, t) => 1 + size(t)
  }

  /*
  def drop[A](l: List[A], n: Int): List[A] = {
    @tailrec
    def go(rl: List[A], rn: Int): List[A] = {
      if (rn <= 0) rl
      else go(tail(rl), rn - 1)
    }

    go(l, n)
  }
  */

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (n <= 0) Cons(h, t)
      else drop(t, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) dropWhile(tail(l), f)
      else t
  }
}
