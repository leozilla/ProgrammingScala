package chapter7

import java.util.concurrent._

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

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

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] =
    executor => {
      logger.debug("fork task on executor")
      executor.submit(new Callable[A] {
        def call = {
          logger.debug("Executing task in executor thread")
          a(executor).get
        }
      })
    }

  def unit[A](a: A): Par[A] = (executor: ExecutorService) => UnitFuture(a)

  def run[A](executor: ExecutorService)(a: Par[A]): Future[A] = {
    logger.debug("run")
    a(executor)
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


