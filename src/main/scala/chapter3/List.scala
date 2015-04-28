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
    foldLeft(as, 0)((_, t) => t + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil:List[A])((x, xs) => Cons(x, xs))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((b, a) => Cons(b, a))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, List[B]())((acc, t) => Cons(f(acc), t))

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, List[B]())((a, b) => append(f(a), b))

  def zipWith[A,B,R](as: List[A], bs: List[B])(f: (A, B) => R): List[R] =
    ???

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((a, b) => if (f(a)) Cons(a, b) else b)

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

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as,z)(f)

  def foldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    @tailrec
    def go(a: List[A], acc: B): B = a match {
      case Nil        => acc
      case Cons(h, t) => go(t, f(h, acc))
    }

    go(as, z)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, b) => b + 1)

  def tail[A](as: List[A]): List[A] = as match {
    case Cons(_, t) => t
    case _ => Nil
  }

  // when passing Nil, MatchError will be generated
  def setHead[A](as: List[A], h: A): List[A] = as match {
    case Cons(_, t) => Cons(h, t)
  }

  def setHead2[A](as: List[A], h: A): List[A] =
    Cons(h, as)

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

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (n <= 0) Cons(h, t)
      else drop(t, n - 1)
  }


  def dropWhile[A](l: List[A]) (f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => applyFunction(dropWhile(t), f)
    case _                  => l
  }

  def applyFunction[A](d:(A => Boolean) => List[A], f:(A=>Boolean)) = d(f)

  // return all but the last element
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
}