package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](as: Tree[A]): Int = as match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def maximum(as: Tree[Int]): Int = as match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](as: Tree[A]): Int = as match {
    case Leaf(x) => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  def map[A,B](as: Tree[A])(f: A => B): Tree[B] = as match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(leafValueMapper: A => B)(branchValuesZipper: (B,B) => B): B = t match {
    case Leaf(x) => leafValueMapper(x)
    case Branch(l, r) =>                   branchValuesZipper(
      fold(l)(leafValueMapper)(branchValuesZipper),    fold(r)(leafValueMapper)(branchValuesZipper))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _ + 1)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(l => l)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(l => 1)((l, r) => (l max r) + 1)

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(l => Leaf(f(l)): Tree[B])(Branch(_, _))
}