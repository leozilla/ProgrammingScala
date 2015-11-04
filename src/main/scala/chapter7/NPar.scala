package chapter7

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

object Nonblocking {
  trait NFuture[+A] {
    private[chapter7] def apply(k: A => Unit): Unit
  }

  type NPar[+A] = ExecutorService => NFuture[A]

  object NPar {
    val logger = Logger(LoggerFactory.getLogger(NPar.getClass))

    def run[A](es: ExecutorService)(p: NPar[A]): A = {
      val ref = new java.util.concurrent.atomic.AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
      val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
      p(es) { a => {
        logger.debug("Executing continuation")
        ref.set(a)
        latch.countDown
      } } // Asynchronously set the result, and decrement the latch
      latch.await // Block until the `latch.countDown` is invoked asynchronously
      ref.get // Once we've passed the latch, we know `ref` has been set, and return its value
    }

    def unit[A](a: A): NPar[A] =
      es => new NFuture[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    /** A non-strict version of `unit` */
    def delay[A](a: => A): NPar[A] =
      es => new NFuture[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    def fork[A](a: => NPar[A]): NPar[A] =
      es => new NFuture[A] {
        def apply(cb: A => Unit): Unit = {
          logger.debug("Forking")
          eval(es)(a(es)(cb))
        }
      }

    /**
     * Helper function for constructing `NPar` values out of calls to non-blocking continuation-passing-style APIs.
     * This will come in handy in Chapter 13.
     */
    def async[A](f: (A => Unit) => Unit): NPar[A] = es => new NFuture[A] {
      def apply(k: A => Unit) = f(k)
    }

    /**
     * Helper function, for evaluating an action
     * asynchronously, using the given `ExecutorService`.
     */
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })

    def map2[A,B,C](p: NPar[A], p2: NPar[B])(f: (A,B) => C): NPar[C] =
      es => new NFuture[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None
          // this implementation is a little too liberal in forking of threads -
          // it forks a new logical thread for the actor and for stack-safety,
          // forks evaluation of the callback `cb`
          val combiner = Actor[Either[A,B]](es) {
            case Left(a) =>
              if (br.isDefined) eval(es)(cb(f(a,br.get)))
              else ar = Some(a)
            case Right(b) =>
              if (ar.isDefined) eval(es)(cb(f(ar.get,b)))
              else br = Some(b)
          }
          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

    // specialized version of `map`
    def map[A,B](p: NPar[A])(f: A => B): NPar[B] =
      es => new NFuture[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => eval(es) { cb(f(a)) })
      }

    def lazyUnit[A](a: => A): NPar[A] =
      fork(unit(a))

    def asyncF[A,B](f: A => B): A => NPar[B] =
      a => lazyUnit(f(a))

    def sequenceRight[A](as: List[NPar[A]]): NPar[List[A]] =
      as match {
        case Nil => unit(Nil)
        case h :: t => map2(h, fork(sequence(t)))(_ :: _)
      }

    def sequenceBalanced[A](as: IndexedSeq[NPar[A]]): NPar[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[NPar[A]]): NPar[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A,B](as: List[A])(f: A => B): NPar[List[B]] =
      sequence(as.map(asyncF(f)))

    def parMap[A,B](as: IndexedSeq[A])(f: A => B): NPar[IndexedSeq[B]] =
      sequenceBalanced(as.map(asyncF(f)))

    // exercise answers

    /*
     * We can implement `choice` as a new primitive.
     *
     * `p(es)(result => ...)` for some `ExecutorService`, `es`, and
     * some `NPar`, `p`, is the idiom for running `p`, and registering
     * a callback to be invoked when its result is available. The
     * result will be bound to `result` in the function passed to
     * `p(es)`.
     *
     * If you find this code difficult to follow, you may want to
     * write down the type of each subexpression and follow the types
     * through the implementation. What is the type of `p(es)`? What
     * about `t(es)`? What about `t(es)(cb)`?
     */
    def choice[A](p: NPar[Boolean])(t: NPar[A], f: NPar[A]): NPar[A] =
      es => new NFuture[A] {
        def apply(cb: A => Unit): Unit =
          p(es) { b =>
            if (b) eval(es) { t(es)(cb) }
            else eval(es) { f(es)(cb) }
          }
      }

    /* The code here is very similar. */
    def choiceN[A](p: NPar[Int])(ps: List[NPar[A]]): NPar[A] =
      es => new NFuture[A] {
        def apply(cb: A => Unit): Unit =
          p(es) { ind => eval(es) { ps(ind)(es)(cb) }}
      }

    def choiceViaChoiceN[A](a: NPar[Boolean])(ifTrue: NPar[A], ifFalse: NPar[A]): NPar[A] =
      choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

    def choiceMap[K,V](p: NPar[K])(ps: Map[K,NPar[V]]): NPar[V] =
      es => new NFuture[V] {
        def apply(cb: V => Unit): Unit =
          p(es)(k => ps(k)(es)(cb))
      }

    /* `chooser` is usually called `flatMap` or `bind`. */
    def chooser[A,B](p: NPar[A])(f: A => NPar[B]): NPar[B] =
      flatMap(p)(f)

    def flatMap[A,B](p: NPar[A])(f: A => NPar[B]): NPar[B] =
      es => new NFuture[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => f(a)(es)(cb))
      }

    def choiceViaFlatMap[A](p: NPar[Boolean])(f: NPar[A], t: NPar[A]): NPar[A] =
      flatMap(p)(b => if (b) t else f)

    def choiceNViaFlatMap[A](p: NPar[Int])(choices: List[NPar[A]]): NPar[A] =
      flatMap(p)(i => choices(i))

    def join[A](p: NPar[NPar[A]]): NPar[A] =
      es => new NFuture[A] {
        def apply(cb: A => Unit): Unit =
          p(es)(p2 => eval(es) { p2(es)(cb) })
      }

    def joinViaFlatMap[A](a: NPar[NPar[A]]): NPar[A] =
      flatMap(a)(x => x)

    def flatMapViaJoin[A,B](p: NPar[A])(f: A => NPar[B]): NPar[B] =
      join(map(p)(f))

    /* Gives us infix syntax for `NPar`. */
    implicit def toNParOps[A](p: NPar[A]): NParOps[A] = new NParOps(p)

    // infix versions of `map`, `map2` and `flatMap`
    class NParOps[A](p: NPar[A]) {
      def map[B](f: A => B): NPar[B] = NPar.map(p)(f)
      def map2[B,C](b: NPar[B])(f: (A,B) => C): NPar[C] = NPar.map2(p,b)(f)
      def flatMap[B](f: A => NPar[B]): NPar[B] = NPar.flatMap(p)(f)
      def zip[B](b: NPar[B]): NPar[(A,B)] = p.map2(b)((_,_))
    }
  }
}