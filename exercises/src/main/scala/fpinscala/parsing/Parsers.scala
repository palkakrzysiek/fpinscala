package fpinscala.parsing

import org.scalatest.Matchers

import language.higherKinds
import language.implicitConversions

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = ??? // string(c.toString)

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = ParserOps(f(a))


  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] //=
//    if (n <= 0) succeed(List())
//    else map2(p, listOfN(n-1, p))(_ :: _)

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
  }

  object Laws {

    import org.scalatest.prop.GeneratorDrivenPropertyChecks._
    import Matchers._

    def or = forAll("s1", "s2"){(s1: String, s2:String) =>
      run(s1 | s2)(s1) should be (Right(s1))
      run(s1 | s2)(s2) should be (Right(s1))
      run(s1 | s2)(s2) should be (Right(s1))
    }

//    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
//    forAll(in)(s => run(p1)(s) == run(p2)(s))
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}