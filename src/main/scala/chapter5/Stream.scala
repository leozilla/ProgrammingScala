package chapter5

sealed trait Stream[+A] {
  def toListRecursive: List[A] = this match {
    case Cons(h,t) => h() :: t().toListRecursive
    case _ => List()
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t() takeWhile p)
    case _                    => Stream.empty
  }

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, t) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))

  def exists(f: A => Boolean): Boolean =
    foldRight(true)((h, t) => f(h) || t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => Stream.cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => f(h).append(t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) =>
      if (f(h)) Stream.cons(h, t)
      else t)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def foldRightTrace[B](z: => B)(f: (A, => B) => B): B = {
    var acc = if (isEmpty) "" else head.toString
    def newAcc(acc: String, x: () => A) = acc + " + (" + x
    def rightSide(xs: Stream[A], z: B, size: Int) = xs.toString + "," + z + ")" * (size - size + 1)
    def printDebug(left: String, right: String) = println(left + " + foldRight(" + right)

    def go(la: Stream[A], z: B)(f: (A, => B) => B): B = la match {
      case Empty => z
      case Cons(x, xs) => {
        acc = newAcc(acc, x)
        printDebug(acc, rightSide(xs(), z, la.size))
        f(x(), go(xs(), z)(f))
      }
    }
    if (isEmpty) z
    else f(head, go(tail, z)(f))
  }

  def size: Int =
    foldRight(0)((x, y) => y + 1)

  def head: A = this match {
    case Cons(h,t) => h()
  }

  def tail: Stream[A] = this match {
    case Cons(h,t) => t()
  }

  def isEmpty: Boolean = this match {
    case Cons(_,_) => false
    case _ => true
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((x, y) => y && p(x))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h,t) => Some(f(h()), t())
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (Cons(h,t), elementsLeft) if elementsLeft > 0 => Some(h(), (t(), elementsLeft - 1))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h,t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s => Some(s, s.drop(1))
    } append Empty

  def hasSubsequence[A](s: Stream[A]): Boolean = ???

  def startsWith[A](s: Stream[A]): Boolean = ???

  def zipAllViaUnfold[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = ???
  /*
    Stream.unfold((this, s2)) {
      case (Cons(hl,tl), Cons(hr,tr)) => Some(Some(hl(), Some(hr)), (tl(), tr()))
      case (Cons(hl,tl), _) => None
      case (_, Cons(hr,tr)) => None
      case (_, _) => None
    }*/

  //Stream.unfold(this){ case (a, s) => Some(Stream.cons(f(a), mapViaUnfold(f)), ) }

  // map, take, takeWhile, zipWith

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => Stream.cons(h, unfold(s)(f))
      case None         => Stream.empty
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // special case of `zip`
  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty)               => None
      case (Cons(h, t), Empty)          => Some(f(Some(h()), Option.empty[B]) -> (t(), Stream.empty[B]))
      case (Empty, Cons(h, t))          => Some(f(Option.empty[A], Some(h())) -> (Stream.empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(s => Some(s, s))

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some(s, s + 1))

  def fibs(): Stream[Int] = {
    def fibs(previous: Int, current: Int): Stream[Int] =
      Stream.cons(previous, fibs(current, previous + current))

    fibs(0, 1)
  }

  def fibsViaUnfold(): Stream[Int] =
    unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2)))
    //unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map(s => cons(s._1, unfold(s._2)(f))).getOrElse(empty)

  /*
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }*/

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}