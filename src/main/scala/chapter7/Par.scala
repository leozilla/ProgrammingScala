package chapter7

import java.util.concurrent.ExecutorService

trait Par[A] {
  def map2[B, C](a: Par[A], b: Par[B]): Par[C]
}

class StrictPar[A](eval: => A) extends Par[A] {


  override def map2[B, C](a: Par[A], b: Par[B]): Par[C] = ???
}
