package chapter7raph

import java.util.concurrent._

import scala.Option._

object RPar {

  val logger = (message: String) => println("log @" + new java.text.SimpleDateFormat("ss.SSS").format(new java.util.Date()) + ": " + message)
  // val logger = (message: String) => Unit

  type RPar[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: RPar[A]): Future[A] = a(s)

  def unit[A](a: A): RPar[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): RPar[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {

    // constructor parameter 'get' implicitly creates a field called 'get'
    // it seems that this field is used for the implementation of 'Future.get()' (without parameters)

    def get(timeout: Long, units: TimeUnit) = get

    def isDone = true
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: RPar[A], b: RPar[B])(f: (A, B) => C): RPar[C] =

    (es: ExecutorService) => {

      val af = a(es)
      val bf = b(es)

      UnitFuture(f(af.get, bf.get))
    }

  def map[A, B](pa: RPar[A])(f: A => B): RPar[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def flatMapSimple[A, B](pa: RPar[A])(f: A => RPar[B]): RPar[B] = es => {
    val a = pa(es).get
    f(a)(es)
  }

  def flatMap[A, B](pa: RPar[A])(f: A => RPar[B]): RPar[B] = {
    val result = map(pa)(f)
    join(result)
  }

  def join[A](pa: RPar[RPar[A]]): RPar[A] = es => {
    val innerParA = pa(es).get
    innerParA(es)
  }

  def joinViaFlatMap[A](ppa: RPar[RPar[A]]): RPar[A] = flatMap(ppa)(identity)

  // runs a, then b!
  def map2Serial[A, B, C](pa: RPar[A], pb: RPar[B])(f: (A, B) => C): RPar[C] =
    flatMap(pa)(a => flatMap(pb)(b => unit(f(a, b))))

  def map2Serial2[A, B, C](pa: RPar[A], pb: RPar[B])(f: (A, B) => C): RPar[C] =
    flatMap(pa)(a => map(pb)(b => f(a, b)))

  def fork[A](a: => RPar[A]): RPar[A] = es => {

    val parent = Thread.currentThread().getName()

    es.submit(new Callable[A] {
      def call = {

        val child = Thread.currentThread().getName
        logger(s"spawned '$child' from '$parent', start calculation")

        val result = a(es).get
        logger(s"finished '$child' with result '$result'")

        result
      }
    })
  }

  def delay[A](fa: => RPar[A]): RPar[A] =
    es => fa(es)

  def asyncF[A, B](f: A => B): A => RPar[B] = a => lazyUnit(f(a))

  def sequence[A](ps: List[RPar[A]]): RPar[List[A]] =
    ps.foldRight[RPar[List[A]]](unit(List()))((x, acc) => map2(x, acc)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): RPar[List[B]] = fork {
    val fbs: List[RPar[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilterNaive[A](as: List[A])(f: A => Boolean): RPar[List[A]] = asyncF(as.filter)(f)

  def parFilter[A](as: List[A])(f: A => Boolean): RPar[List[A]] = fork {

    val listOfRParOfOptions = as.map(asyncF(a => if (f(a)) Some(a) else None))
    val parOfListOfOptions = sequence(listOfRParOfOptions)

    // map() is RPar.map()
    // flatten() transforms lists of lists into single lists by concat-ing them
    // it also works for a lists of Options since Options can behave as empty or 1-element lists via its GenTraversable implementation
    map(parOfListOfOptions)(xs => xs.flatten)

    // alternative implementations:
    // map(parOfListOfOptions)(xs => xs.flatMap(x => x))
    // map(parOfListOfOptions)(xs => xs.filter(x => x.isDefined).map(x => x.get))
  }

  def sum(ints: IndexedSeq[Int]): RPar[Int] =
    fold(ints)(_ + _, 0)(2)

  def fold[A](seq: IndexedSeq[A])(f: (A, A) => A, default: A)(forkUntil: Integer): RPar[A] =
    if (seq.length <= 1)
      unit(seq.headOption getOrElse default)
    else {
      val (l, r) = seq.splitAt(seq.length / 2)
      if (seq.length > forkUntil)
        map2(fork(fold(l)(f, default)(forkUntil)), fork(fold(r)(f, default)(forkUntil)))(f)
      else
        map2(fold(l)(f, default)(forkUntil), fold(r)(f, default)(forkUntil))(f)
    }

  def max(ints: IndexedSeq[Int])(forkUntil: Integer): RPar[Int] = fold(ints)(Math.max, 0)(forkUntil)

  def countWords(paragraphs: List[String]): RPar[Int] = {
    val words = parMap(paragraphs)(x => x.split(" ").length)
    flatMap(words)(x => sum(x.toIndexedSeq))
  }

  def map3ViaFlatMap[A, B, C, D](a: RPar[A], b: RPar[B], c: RPar[C])(f: (A, B, C) => D): RPar[D] =
    flatMap(a)(va => flatMap(b)(vb => map(c)(vc => f(va, vb, vc))))

  def map3[A, B, C, D](a: RPar[A], b: RPar[B], c: RPar[C])(f: (A, B, C) => D): RPar[D] = {

    val curriedFunction = f.curried

    val intermediate = map2(a, b)((a, b) => curriedFunction(a)(b))
    map2(intermediate, c)((function, c) => function(c))
  }

  def map3Compact[A, B, C, D](a: RPar[A], b: RPar[B], c: RPar[C])(f: (A, B, C) => D): RPar[D] = map2(map2(a, b)(f.curried(_)(_)), c)(_.apply(_))

  def map4Compact[A, B, C, D, E](a: RPar[A], b: RPar[B], c: RPar[C], d: RPar[D])(f: (A, B, C, D) => E): RPar[E] =
    map2(map2(map2(a, b)(f.curried(_)(_)), c)(_.apply(_)), d)(_.apply(_))

  def map5Compact[A, B, C, D, E, F](a: RPar[A], b: RPar[B], c: RPar[C], d: RPar[D], e: RPar[E])(f: (A, B, C, D, E) => F): RPar[F] =
    map2(map2(map2(map2(a, b)(f.curried(_)(_)), c)(_.apply(_)), d)(_.apply(_)), e)(_.apply(_))

  def choiceN[A](n: RPar[Int])(choices: List[RPar[A]]): RPar[A] = es => {
    val index = run(es)(n).get
    choices(index)(es)
  }

  def choice[A](cond: RPar[Boolean])(t: RPar[A], f: RPar[A]): RPar[A] =
    choiceN(map(cond)(v => if (v) 0 else 1))(List(t, f))

  def choiceMap[K, V](key: RPar[K])(choices: Map[K, RPar[V]]): RPar[V] = es =>
  {
    val computedKey = run(es)(key).get
    choices(computedKey)(es)
  }

  def chooser[A, B](pa: RPar[A])(choices: A => RPar[B]): RPar[B] =
    flatMap(pa)(choices)

}
