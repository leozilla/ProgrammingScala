package chapter4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def flatMapViaPattern[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def orElseViaPattern[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] =
    flatMap((a) => if (f(a)) Some(a) else None)

  def filterViaPattern(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  def map2ViaPattern[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(aa), Some(bb)) => Some(f(aa, bb))
    case _ => None
  }

  def map2ViaFor[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil):Option[List[A]])((value, acc) => value.flatMap(v => acc.map(a => v::a)))

  def sequenceViaPattern2[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil        => Some(Nil)
    case Some(h)::t => sequenceViaPattern2(t).map(xs => h :: xs)
    case _          => None
  }

  def sequenceViaPattern3[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil  => Some(Nil)
    case h::t => h flatMap (hh => sequenceViaPattern2(t).map(xs => hh :: xs))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))
  //a.foldRight[Option[List[A]]](Some(Nil))((optionalA, b) => optionalA.flatMap(a1 => b map (bb => a1::bb)))

  def sequenceViaPattern[A](a: List[Option[A]]): Option[List[A]] = a match {
    case h::t  => map2(h, sequenceViaPattern(t))((a, b) => a::b)
    case Nil => Some(Nil)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2(f(x),y)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(aa => aa)
}

