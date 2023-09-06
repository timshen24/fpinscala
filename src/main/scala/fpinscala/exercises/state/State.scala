package fpinscala.exercises.state

import scala.annotation.tailrec


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i: Int, rng2: RNG) = rng.nextInt
    (if i == Int.MinValue then Int.MaxValue else i.abs, rng2)

  def double(rng: RNG): (Double, RNG) =
    val (i: Int, rng2: RNG) = nonNegativeInt(rng)
    val j = if i == Int.MaxValue then (i - 1).toDouble else i.toDouble
    (j / Int.MaxValue, rng2)

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val (i, rng2) = nonNegativeInt(rng)
    val (d, rng3) = double(rng2)
    ((i, d), rng3)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val ((i,d), rng2) = intDouble(rng)
    ((d, i), rng2)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    @tailrec
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if count <= 0 then
        (xs, r)
      else
        val (x, r1) = r.nextInt
        go(count - 1, r1, x :: xs)
    go(count, rng, List.empty[Int])

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = ???

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = ???

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      ???

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      ???

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      ???

  def apply[S, A](f: S => (A, S)): State[S, A] = f

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
