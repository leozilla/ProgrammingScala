package chapter7

import java.util.concurrent._

/*
abstract class ExecutorService {
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

  type Par[A] = ExecutorService => Future[A]

  def map[A, B](a: Par[A])(f: A => B): Par[B] =
    map2(a, unit())((a, _) => f(a))

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val aFuture = a(es)
      val bFuture = b(es)
      UnitFuture(f(aFuture.get, bFuture.get))
    }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def run[A](p: Par[A]): A = ???

  private case class UnitFuture[A](get: A) extends Future[A] {

    override def isCancelled: Boolean = false

    override def get(timeout: Long, unit: TimeUnit): A = get

    override def cancel(evenIfRunning: Boolean): Boolean = false

    override def isDone: Boolean = true
  }
}


