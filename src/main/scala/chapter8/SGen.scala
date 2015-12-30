package chapter8

case class SGen[A](sizeToGen: Int => Gen[A]) {

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen[B](n => sizeToGen(n) flatMap (a => f(a).sizeToGen(n)))

  def apply(i: Int) = sizeToGen(i)

  def listOfN(sizeGen: SGen[Int]): SGen[List[A]] =
    SGen(n => sizeGen.flatMap(sizeN => SGen(_ => Gen.listOfN(sizeN, sizeToGen(n))))(n))

}

object SGen {

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(n, g))


}
