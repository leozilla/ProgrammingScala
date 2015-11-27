package chapter8

case class SGen[A](forSize: Int => Gen[A]) {

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => forSize(n).flatMap(a => f(a).forSize(n)))

  def listOfN(sizeGen: SGen[Int]): SGen[List[A]] =
    ???
}

object SGen {

}