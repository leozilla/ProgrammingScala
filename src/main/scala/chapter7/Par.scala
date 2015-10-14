package chapter7

import java.util.concurrent._

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.concurrent.duration.Duration

/*
class ExecutorService {
  def submit[A](a: Callable[A]): Future[A]
}
trait Callable[A] { def call: A }

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}*/

object Par {
  val logger = Logger(LoggerFactory.getLogger(Par.getClass))

  type Par[A] = ExecutorService => Future[A]

  def map[A, B](a: Par[A])(f: A => B): Par[B] =
    map2(a, unit())((a, _) => f(a))

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (executor: ExecutorService) => {
      logger.debug("map2")
      val aFuture = a(executor)
      val bFuture = b(executor)
      UnitFuture(f(aFuture.get, bFuture.get))
    }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A,B,C) => D): Par[D] = {
    val res: C => Par[D] = cc => map2(a, b)((aa, bb) => f(aa, bb, cc))
    map(c)(res)
    ???
  }

  def map2TimeoutAware[A, B, C](a: Par[A], b: Par[B], t: Duration)(f: (A,B) => C): Par[C] =
    (executor: ExecutorService) => {
      logger.debug("map2")
      val aFuture = a(executor)
      val bFuture = b(executor)

      val start = System.nanoTime()
      val computedA = aFuture.get(t.toNanos, TimeUnit.NANOSECONDS)
      val elapsed = System.nanoTime() - start
      val remaining = t.toNanos - elapsed
      if (remaining <= 0) {
        throw new TimeoutException("wait timed out")
      }

      val computedB = bFuture.get(remaining, TimeUnit.NANOSECONDS)

      UnitFuture(f(computedA, computedB))
    }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] =
    executor => {
      logger.debug("fork task on executor")
      val f = executor.submit(new Callable[A] {
        def call = {
          logger.debug("Executing task in executor thread")
          val result = a(executor).get
          logger.debug("Task calculation finished")
          result
        }
      })
      logger.debug("forked task on executor")
      f
    }

  def unit[A](a: A): Par[A] = (executor: ExecutorService) => UnitFuture(a)

  def run[A](executor: ExecutorService)(a: Par[A]): Future[A] = {
    logger.debug("run")
    a(executor)
  }

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(l => l.sorted)

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val asyncAs: List[Par[B]] = ps.map(asyncF(f))
    sequence(asyncAs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight[Par[List[A]]](unit(List()))((p, acc) => map2(p, acc)(_ :: _))
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val asyncOptions = as.map(asyncF(a2 => if (f(a2)) Some(a2) else None))
    map(sequence(asyncOptions))(l => l.flatten)
  }

  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      sum(l) + sum(r)
    }

  private case class UnitFuture[A](a: A) extends Future[A] {

    override def isCancelled: Boolean = false

    override def get(timeout: Long, unit: TimeUnit): A = {
      logger.debug("Getting value with timeout")
      a
    }

    override def cancel(evenIfRunning: Boolean): Boolean = false

    override def isDone: Boolean = true

    override def get(): A = {
      logger.debug("Getting value blocking")
      a
    }
  }
}


