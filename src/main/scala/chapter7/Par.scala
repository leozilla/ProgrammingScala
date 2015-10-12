package chapter7

import java.util.concurrent.{TimeUnit, ExecutorService}

trait Callable[A] { def call: A }

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def map2[A, B, C](a: Par[A], b: Par[B]): Par[C] = ???

  def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => ParFuture(es, a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {

    override def isCancelled: Boolean = false

    override def get(timeout: Long, unit: TimeUnit): A = get

    override def cancel(evenIfRunning: Boolean): Boolean = false

    override def isDone: Boolean = true
  }

  private case class ParFuture[A](es: ExecutorService, lazyA: => Par[A]) extends Future[A] {

    override def get: A = ???

    override def isCancelled: Boolean = ???

    override def get(timeout: Long, unit: TimeUnit): A = ???

    override def cancel(evenIfRunning: Boolean): Boolean = ???

    override def isDone: Boolean = ???
  }
}


