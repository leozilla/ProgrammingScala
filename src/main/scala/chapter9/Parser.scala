package chapter9

trait Parser[A]

trait Parsers[ParseError, Parser[+_]] {
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  def char(c: Char): Parser[Char]
  def string(s: String): Parser[String]
}

object Parser {

}
