package chapter3

sealed trait Tree[+A]
case object NilTree extends Tree[Nothing]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](as: Tree[A]): Int = as match {
    case NilTree => 0
    case Leaf(_) => 1
    case Branch(l, Leaf(NilTree)) => size(l) + 1
    case Branch(Leaf(NilTree), r) => size(r) + 1
  }

  /*
  def apply[A](as: A*): Tree[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
    */
}