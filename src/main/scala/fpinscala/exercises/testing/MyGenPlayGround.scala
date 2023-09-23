package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.testing.MyGen.*
import fpinscala.exercises.testing.MyProp.*

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

opaque type TestCases = Int
object TestCases:
  extension (x: TestCases) def toInt: Int = x
  def fromInt(x: Int): TestCases = x

opaque type MyProp = (TestCases, RNG) => Result

opaque type FailedCase = String

object FailedCase:
  extension (f: FailedCase) def string: String = f

  def fromString(s: String): FailedCase = s

enum Result:
  case Passed
  case Falsified(failure: FailedCase, successes: SuccessCount)

  def isFalsified: Boolean = this match
    case Passed => false
    case Falsified(_, _) => true


//  def &&(that: MyProp): MyProp = new MyProp {
//    override def check: Boolean = self.check && that.check
//  }

object MyProp:
  opaque type SuccessCount = Int

  import Result.*
  def forAll[A](as: MyGen[A])(f: A => Boolean): MyProp =
    def randomLazyList[A](g: MyGen[A])(rng: RNG): LazyList[A] =
      LazyList.unfold(rng)(rng => Some(g.run(rng)))

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
    (n, rng) =>
      randomLazyList(as)(rng)
        .zip(LazyList.from(0))
        .take(n)
        .map:
          case (a, i) =>
            try
              if f(a) then Passed
              else Falsified(a.toString, i)
            catch
              case e: Exception =>
                Falsified(buildMsg(a, e), i)
        .find(_.isFalsified)
        .getOrElse(Passed)


  extension (self: MyProp) def &&(that: MyProp): MyProp =
    (n, rng) => self.tag("and-left")(n, rng) match
      case Result.Passed => that.tag("and-right")(n, rng)
      case x => x

  extension (self: MyProp) def ||(that: MyProp): MyProp =
    (n, rng) => self.tag("or-left")(n, rng) match
      case Result.Falsified(failure, _) => that.tag(failure)(n, rng)
      case x => x

  extension (self: MyProp)
    def run(): Unit =
      self(100, RNG.Simple(System.currentTimeMillis)) match
        case Result.Passed => println(s"+ OK, passed 100 test.")
        case Result.Falsified(failure, successes) => println(s"! Falsified after $successes passed tests:\n $failure")

  extension (self: MyProp)
    def tag(msg: String): MyProp =
      (n, rng) => self(n, rng) match
        case Result.Falsified(failure, successes) => Falsified(FailedCase.fromString(s"$msg($failure)"), successes)
        case x => x

  @main def Main: Unit =
    val p: MyProp = MyProp.forAll(MyGen.boolean)(x => x == x)
    val q: MyProp = MyProp.forAll(MyGen.boolean)(x => x)
    (p && q).run()
    (q && p).run()
    (q || q).run()


opaque type MyGen[+A] = State[RNG, A]

opaque type MySGen[/*+*/A] = Int => MyGen[A]

object MyGen {
  // 根据返回值知道MyGen其实是一个State，所以要得到一个MyGen，就干脆返回一个State对象就可以了？最方便的方式是用State.apply的方法
  def choose(start: Int, stopExclusive: Int): MyGen[Int] = State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))

  def unit[A](a: => A): MyGen[A] = State(RNG.unit(a))

  def boolean: MyGen[Boolean] = State(RNG.boolean)

  extension[A] (self: MyGen[A])
    // sequence是亮点
     def listOfN0(n: Int): MyGen[List[A]] = State.sequence(List.fill(n)(self))

  extension[A] (self: MyGen[A]) def map[B](f: A => B): MyGen[B] =
    State.map(self)(f)

  extension[A] (self: MyGen[A]) def flatMap[B](f: A => MyGen[B]): MyGen[B] =
    State.flatMap(self)(f)

  extension[A] (self: MyGen[A]) def listOfN(size: MyGen[Int]): MyGen[List[A]] =
    size.flatMap(self.listOfN0)

  def union[A](g1: MyGen[A], g2: MyGen[A]): MyGen[A] =
    boolean.flatMap(b => if b then g1 else g2)

  def weighted[A](g1: (MyGen[A], Double), g2: (MyGen[A], Double)): MyGen[A] =
    val (gen1, p1) = g1
    val (gen2, p2) = g2
    val g1Threshold = p1 / (p1 + p2)
    State(RNG.double).flatMap(d => if d <= g1Threshold then gen1 else gen2)

  extension [A](self: MyGen[A]) def unsized: MySGen[A] = _ => self
}

object MySGen {
  import MyGen.*
  extension [A](self: MySGen[A]) def map[B](f: A => B): MySGen[B] = n =>
    self(n).map(f)

  extension [A](self: MySGen[A]) def flatMap[B](f: A => MySGen[B]): MySGen[B] = n =>
    self(n).flatMap(a => f(a)(n))

  extension [A](self: MyGen[A]) def list: MySGen[List[A]] = n =>
    self.listOfN0(n)
}