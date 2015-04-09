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

  def product(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def product2(ds: List[Double]): Double =
    foldRight(ds, 1.0)((a, b) =>
      if (a != 0.0) a * b
      else 0.0)

  def productLeft(ds: List[Double]): Double =
    foldLeft(ds, 1.0d)(_ * _)

  def sumLeft(ints: List[Double]): Double =
    foldLeft(ints, 0d)(_ + _)

  def lengthLeft(as: List[Any]): Int =
    foldLeft(as, 0)((a, _) => a + 1)

  def reverse(as: List[Any]): List[Any] =
    foldLeft(as, List[Any]())((b, a) => Cons(a, b))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((b, a) => Cons(b, a))

  def map(as: List[Int], f: Int => Int): List[Int] =
    foldRight(as, List[Int]())((acc, t) => Cons(f(acc), t))

  /*
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  */

  def foldRight[A,B](l: List[A], nilValue: B)(f: (A, B) => B): B = l match {
    case Nil => nilValue
    case Cons(x,xs) => f(x, foldRight(xs, nilValue)(f))
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(a: List[A], acc: B): B = {
      if (a == Nil) acc
      else go(tail(a), f(acc, head(a)))
    }

    go(as, z)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, b) => b + 1)

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

  // return all but the last element
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
}