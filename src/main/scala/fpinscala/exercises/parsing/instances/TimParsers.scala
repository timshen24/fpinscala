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



