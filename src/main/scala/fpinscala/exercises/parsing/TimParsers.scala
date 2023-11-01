package fpinscala.exercises.parsing

import fpinscala.answers.testing.*
import fpinscala.answers.testing.Prop.forAll

import scala.util.matching.Regex

trait TimParsers[TimParser[+_]]:
  self => // so inner classes may call methods of trait
  def char(c: Char): TimParser[Char] =
    string(c.toString).map(_.charAt(0))

  def string(s: String): TimParser[String]

  def succeed[A](a: A): TimParser[A] =
    string("").map(_ => a)

  def fail(msg: String): TimParser[Nothing]

  def defer[A](p: => TimParser[A]): TimParser[A]

  def regex(r: Regex): TimParser[String]

  extension [A](p: TimParser[A])

    def run(input: String): Either[TimParseError, A]

    infix def or(p2: => TimParser[A]): TimParser[A]
    def |(p2: => TimParser[A]): TimParser[A] = p.or(p2)

    def listOfN(n: Int): TimParser[List[A]] = {
      if n == 0 then succeed(Nil) else p.map2(listOfN(n - 1))(_ :: _)
    }

    def many: TimParser[List[A]] = p.map2(p.many)(_ :: _) | succeed(Nil)

    def map[B](f: A => B): TimParser[B] =
      p.flatMap(a => succeed(f(a)))

    def slice: TimParser[String]

    def product[B](p2: TimParser[B]): TimParser[(A, B)] =
      for
        a <- p
        b <- p2
      yield (a, b)

    def **[B](p2: TimParser[B]): TimParser[(A, B)] = product(p2)

    def map2[B, C](p2: => TimParser[B])(f: (A, B) => C): TimParser[C] =
      for
        a <- p
        b <- p2
      yield f(a, b)

    def many1: TimParser[List[A]] = (p ** p.many).map(_ :: _)

    infix def ~=[B, C](p1: TimParser[((A, B), C)], p2: TimParser[(A, (B, C))]) =
      import Laws.*
      p1.map(unbiasL) == p2.map(unbiasR)

    def flatMap[B](f: A => TimParser[B]): TimParser[B]

    def label(msg: String): TimParser[A]

    def scope(msg: String): TimParser[A]

    def attempt: TimParser[A]

    /** In the event of an error, returns the error that occurred after consuming the most number of characters. */
    def furthest: TimParser[A]

    /** In the event of an error, returns the error that occurred most recently. */
    def latest: TimParser[A]

  case class ErrorLocation(input: String, offset: Int = 0):
    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match
      case -1 => offset + 1
      case lineStart => offset - lineStart

  def errorLocation(e: TimParseError): TimLocation

  def errorMessage(e: TimParseError): String

  val spaces: TimParser[List[String]] = string(" ").many

  val nonNegativeInt: TimParser[Int] =
    for
      nString <- regex("[0-9]+".r)
      n <- nString.toIntOption match
        case Some(n) => succeed(n)
        case None => fail("expected an integer")
    yield n

  val nConsecutiveAs: TimParser[Int] =
    for
      n <- nonNegativeInt
      _ <- char('a').listOfN(n)
    yield n

  def playground(): Unit =
    char('a').many.slice.map(_.length) ** char('b').many1.slice.map(_.length)

  case class ParserOps[A](p: TimParser[A])

  object Laws:
    def equal[A](p1: TimParser[A], p2: TimParser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => p1.run(s) == p2.run(s))

    def mapLaw[A](p: TimParser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)

    def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)

    def productLaw[A, B, C](p1: TimParser[A], p2: TimParser[B], p3: TimParser[C])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => ((p1 ** p2) ** p3).map(unbiasL).run(s) == (p1 ** (p2 ** p3)).map(unbiasR).run(s))

    def labelLaw[A](p: TimParser[A], inputs: SGen[String]): Prop =
      forAll(inputs ** Gen.string):
        case (input, msg) =>
          p.label(msg).run(input) match
            case Left(e) => errorMessage(e) == msg
            case _ => true

case class TimLocation(input: String, offset: Int = 0):

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): TimParseError =
    TimParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  def remaining: String = ???

  def slice(n: Int) = ???

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.linesIterator.drop(line-1).next()
    else ""

case class TimParseError(stack: List[(TimLocation,String)] = List(),
                      otherFailures: List[TimParseError] = List()):
  def push(loc: TimLocation, msg: String): TimParseError = copy(stack = (loc, msg) :: stack)

  def latest: Option[(TimLocation, String)] =
    stack.lastOption

  def latestLoc: Option[TimLocation] =
    latest map (_._1)

  def label(s: String): TimParseError = TimParseError(latestLoc.map((_, s)).toList)

class TimExamples[TimParser[+_]](P: TimParsers[TimParser]):
  import P.*

  val nonNegativeInt: TimParser[Int] =
    for
      nString <- regex("[0-9]+".r)
      n <- nString.toIntOption match
        case Some(n) => succeed(n)
        case None => fail("expected an integer")
    yield n

  val nonNegativeIntOpaque: TimParser[Int] =
    nonNegativeInt.label("non-negative integer")

  val nConsecutiveAs: TimParser[Int] = ???
