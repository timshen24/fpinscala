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
//    val (i: Int, rng2: RNG) = nonNegativeInt(rng)
//    val j = if i == Int.MaxValue then (i - 1).toDouble else i.toDouble
//    (j / Int.MaxValue, rng2)
//    Use map to reimplement double in a more succinct way.
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))(rng)

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

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(Nil: List[A]))((r, rngs) => map2(r, rngs)(_ :: _))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (x: A, rng1: RNG) = r(rng)
      f(x)(rng1)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt): i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0 then unit(mod)
      else nonNegativeLessThan(n)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
//    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      underlying.flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s1) = underlying(s)
        f(a)(s1)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] =
    s => (a, s)

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(unit[S, List[A]](Nil))((a, as) => a.map2(as)(_ :: _))

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight(unit[S, List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    val update = (i: Input) => (s: Machine) =>
      (i, s) match
        case (_, Machine(_, 0, _)) => s
        case (Input.Coin, Machine(false, _, _)) => s
        case (Input.Turn, Machine(true, _, _)) => s
        case (Input.Coin, Machine(true, candy, coin)) =>
          Machine(false, candy, coin + 1)
        case (Input.Turn, Machine(false, candy, coin)) =>
          Machine(true, candy - 1, coin)

    for
      _ <- State.traverse[Machine, Input, Unit](inputs)(i => State.modify(update(i)))
      s <- State.get
    yield (s.coins, s.candies)
