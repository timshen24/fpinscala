package fpinscala.exercises.parsing.instances

import scala.util.matching.Regex
import TimParsers.Result.{Failure, Success}
import fpinscala.exercises.parsing.{TimLocation, TimParsers, TimParseError}

//class TimParser[+A]()
object TimParsers extends TimParsers[TimParsers.TimParser]:

  type TimParser[+A] = TimLocation => Result[A]

  enum Result[+A]:
    case Success(get: A, length: Int)
    case Failure(get: TimParseError, isCommitted: Boolean) extends Result[Nothing]

    /* Used by `scope`, `label`. */
    def mapError(f: TimParseError => TimParseError): Result[A] = this match
      case Failure(e, c) => Failure(f(e), c)
      case _ => this

    /* Used by `attempt`. */
    def uncommit: Result[A] = this match
      case Failure(e, true) => Failure(e, false)
      case _ => this

    /* Used by `flatMap`. */
    def addCommit(isCommitted: Boolean): Result[A] = this match
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _ => this

    def advanceSuccess(n: Int): Result[A] = this match
      case Success(a, m) => Success(a, n + m)
      case _ => this

  /** Returns -1 if s1.startsWith(s2), otherwise returns the
   * first index where the two strings differed. If s2 is
   * longer than s1, returns s1.length. */
  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int =
    var i = 0
    while i + offset < s1.length && i < s2.length do
      if s1.charAt(i + offset) != s2.charAt(i) then return i
      i += 1
    if s1.length - offset >= s2.length then -1
    else s1.length - offset

  def string(w: String): TimParser[String] =
    l =>
      val i = firstNonmatchingIndex(l.input, w, l.offset)
      if i == -1 then // they matched
        Success(w, w.length)
      else
        Failure(l.advanceBy(i).toError(s"'$w'"), i != 0)

  /* note, regex matching is 'all-or-nothing':
   * failures are uncommitted */
  def regex(r: Regex): TimParser[String] =
    l => r.findPrefixOf(l.remaining) match
      case None => Failure(l.toError(s"regex $r"), false)
      case Some(m) => Success(m, m.length)

  // consume no characters and succeed with the given value
  override def succeed[A](a: A): TimParser[A] =
    _ => Success(a, 0)

  override def fail(msg: String): TimParser[Nothing] = ???

  override def defer[A](p: => TimParser[A]): TimParser[A] = ???

  override def errorLocation(e: TimParseError): TimLocation = ???

  override def errorMessage(e: TimParseError): String = ???

  extension [A](p: TimParser[A])
    def slice: TimParser[String] =
      l => p(l) match
        case Success(_, n) => Success(l.slice(n), n)
        case f@Failure(_, _) => f

    def scope(msg: String): TimParser[A] =
      l => p(l).mapError(_.push(l, msg))

    def label(msg: String): TimParser[A] =
      l => p(l).mapError(_.label(msg))

    def attempt: TimParser[A] =
      l => p(l).uncommit

    def or(p2: => TimParser[A]): TimParser[A] =
      l => p(l) match
        case Failure(e, false) => p2(l)
        case r => r

    def flatMap[B](f: A => TimParser[B]): TimParser[B] =
      l => p(l) match
        case Success(a, n) =>
          f(a)(l.advanceBy(n))
            .addCommit(n != 0)
            .advanceSuccess(n)
        case f@Failure(_, _) => f

    /** In the event of an error, returns the error that occurred after consuming the most number of characters. */
    def furthest: TimParser[A] = ???

    /** In the event of an error, returns the error that occurred most recently. */
    def latest: TimParser[A] = ???

    def run(input: String): Either[TimParseError, A] = ???