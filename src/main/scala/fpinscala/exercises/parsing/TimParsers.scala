package fpinscala.exercises.parsing

import fpinscala.answers.testing.*

trait TimParsers[TimParser[+_]]:
  self => // so inner classes may call methods of trait
  def char(c: Char): TimParser[Char] =
    string(c.toString).map(_.charAt(0))

  def string(s: String): TimParser[String]

  def succeed[A](a: A): TimParser[A] =
    string("").map(_ => a)

  def defer[A](p: => TimParser[A]): TimParser[A]

  extension [A](p: TimParser[A])

    def run(input: String): Either[TimParseError, A]

    infix def or(p2: => TimParser[A]): TimParser[A]
    def |(p2: => TimParser[A]): TimParser[A] = p.or(p2)

    def listOfN(n: Int): TimParser[List[A]] = {
      if n == 0 then succeed(Nil) else p.map2(listOfN(n - 1))(_ :: _)
    }

    def many: TimParser[List[A]] = p.map2(p.many)(_ :: _) | succeed(Nil)

    def map[B](f: A => B): TimParser[B]

    def slice: TimParser[String]

    def product[B](p2: TimParser[B]): TimParser[(A, B)]

    def **[B](p2: TimParser[B]): TimParser[(A, B)] = product(p2)

    def map2[B, C](p2: => TimParser[B])(f: (A, B) => C): TimParser[C] = (p ** p2).map((a, b) => f(a, b))

    def many1: TimParser[List[A]] = (p ** p.many).map(_ :: _)

    infix def ~=[B, C](p1: TimParser[((A, B), C)], p2: TimParser[(A, (B, C))]) =
      import Laws.*
      p1.map(unbiasL) == p2.map(unbiasR)

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
  def push(loc: TimLocation, msg: String): TimParseError = ???

  def label(s: String): TimParseError = ???

class TimExamples[TimParser[+_]](P: TimParsers[TimParser]):
  import P.*

  val nonNegativeInt: TimParser[Int] = ???

  val nConsecutiveAs: TimParser[Int] = ???
