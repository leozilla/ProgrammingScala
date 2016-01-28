package chapter9

trait Parser[A]

trait Parsers[ParseError, Parser[+_]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]]
  def map[A,B](a: Parser[A])(f: A => B): Parser[B]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](aParser: Parser[A]) {
    def |[B>:A](bParser: Parser[B]): Parser[B] = self.or(aParser, bParser)
    def or[B>:A](bParser: => Parser[B]): Parser[B] = self.or(aParser, bParser)
    def many: Parser[List[A]] = self.many(aParser)
    def map[B](f: A => B): Parser[B] = self.map(aParser)(f)
  }
}

object Parser {

}
