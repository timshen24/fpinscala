//package fpinscala.exercises.testing
//
//import fpinscala.exercises.state.*
//import fpinscala.exercises.testing.MyGen.*
//import fpinscala.exercises.testing.MyProp.*
//
///*
//The library developed in this chapter goes through several iterations. This file is just the
//shell, which you can fill in and modify while working through the chapter.
//*/
//
//opaque type TestCases = Int
//object TestCases:
//  extension (x: TestCases) def toInt: Int = x
//  def fromInt(x: Int): TestCases = x
//
//opaque type MyProp = (MaxSize, TestCases, RNG) => Result
//
//opaque type FailedCase = String
//
//object FailedCase:
//  extension (f: FailedCase) def string: String = f
//
//  def fromString(s: String): FailedCase = s
//
//opaque type MaxSize = Int
//
//object MaxSize:
//  extension (x: MaxSize) def toInt: Int = x
//  def fromInt(x: Int): MaxSize = x
//
//enum Status:
//  case Proven, Unfalsified
//enum Result:
//  case Passed(status: Status, testCases: TestCases)
//  case Falsified(failure: FailedCase, successes: SuccessCount)
//
//  def isFalsified: Boolean = this match
//    case Falsified(_, _) => true
//    case _ => false
//
//
////  def &&(that: MyProp): MyProp = new MyProp {
////    override def check: Boolean = self.check && that.check
////  }
//
//object MyProp:
//  opaque type SuccessCount = Int
//
//  import Result.*
//  def forAll[A](as: MyGen[A])(f: A => Boolean): MyProp =
//    def randomLazyList[A](g: MyGen[A])(rng: RNG): LazyList[A] =
//      LazyList.unfold(rng)(rng => Some(g.run(rng)))
//
//    def buildMsg[A](s: A, e: Exception): String =
//      s"test case: $s\n" +
//        s"generated an exception: ${e.getMessage}\n" +
//        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
//    (max, n, rng) =>
//      randomLazyList(as)(rng)
//        .zip(LazyList.from(0))
//        .take(n)
//        .map:
//          case (a, i) =>
//            try
//              if f(a) then Passed
//              else Falsified(a.toString, i)
//            catch
//              case e: Exception =>
//                Falsified(buildMsg(a, e), i)
//        .find(_.isFalsified)
//        .getOrElse(Passed)
//
//  def forAllNew[A](g: MySGen[A])(f: A => Boolean): MyProp =
//    (max, n, rng) =>
//      val casesPerSize = (n.toInt - 1) / max.toInt + 1
//      val props: LazyList[MyProp] =
//        LazyList.from(0)
//          .take((n.toInt min max.toInt) + 1)
//          .map(i => forAll(g(i))(f))
//      val prop: MyProp =
//        props.map[MyProp](p => (max, n, rng) =>
//            p(max, casesPerSize, rng))
//          .toList
//          .reduce(_ && _)
//      prop(max, n, rng)
//
//  /**
//   * 专门给hardcode的example用的方法，比如1+1是否等于2，而非forall型的方法，所以不用指定MaxCase等等
//   */
//  def verify(p: => Boolean): MyProp =
//    (_, _, _) => if p then Passed else Falsified("()", 0)
//
//  extension (self: MyProp) def &&(that: MyProp): MyProp =
//    (max, n, rng) => self.tag("and-left")(max, n, rng) match
//      case Result.Passed | Result.Proved => that.tag("and-right")(max, n, rng)
//      case x => x
//
//  extension (self: MyProp) def ||(that: MyProp): MyProp =
//    (max, n, rng) => self.tag("or-left")(max, n, rng) match
//      case Result.Falsified(failure, _) => that.tag(failure)(max, n, rng)
//      case x => x
//
//  extension (self: MyProp)
//    def tag(msg: String): MyProp =
//      (max, n, rng) => self(max, n, rng) match
//        case Result.Falsified(failure, successes) => Falsified(FailedCase.fromString(s"$msg($failure)"), successes)
//        case x => x
//
//  extension (self: MyProp)
//    def check(
//               maxSize: MaxSize = 100,
//               testCases: TestCases = 100,
//               rng: RNG = RNG.Simple(System.currentTimeMillis)
//             ): Result =
//      self(maxSize, testCases, rng)
//
//  extension (self: MyProp)
//    def run(maxSize: MaxSize = 100,
//            testCases: TestCases = 100,
//            rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
//      self(maxSize, testCases, rng) match
//        case Falsified(msg, n) =>
//          println(s"! Falsified after $n passed tests:\n $msg")
//        case Passed =>
//          println(s"+ OK, passed $testCases tests.")
//        case Proved =>
//          println(s"+ OK, proved property.")
//
//  @main def Main: Unit =
//    val p: MyProp = MyProp.forAll(MyGen.boolean)(x => x == x)
//    val q: MyProp = MyProp.forAll(MyGen.boolean)(x => x)
//    (p && q).run()
//    (q && p).run()
//    (q || q).run()
//
//    val smallInt = MyGen.choose(-10, 10)
//    val maxProp = MyProp.forAllNew(smallInt.nonEmptyList): ns =>
//      val max = ns.max
//      ns.forall(_ <= max)
//    maxProp.run()
//
//    val sortedProp = MyProp.forAllNew(smallInt.list): l =>
//      val ls = l.sorted
//      val ordered = l.isEmpty || ls.zip(ls.tail).forall((a, b) => a <= b)
//      ordered && l.forall(ls.contains) && ls.forall(l.contains)
//    sortedProp.run()
//
////opaque type MyGen[+A] = State[RNG, A]
//
//case class MyGen[+A](sample: State[RNG, A], exhaustive: LazyList[Option[A]]) {
//  def map[B](f: A => B): MyGen[B] =
//    MyGen(sample.map(f), exhaustive.map(_.map(f)))
//
//  def map2[B, C](g: MyGen[B])(f: (A, B) => C): MyGen[C] =
//    MyGen(sample.map2(g.sample)(f), map2LazyList(exhaustive, g.exhaustive)(map2Option(_, _)(f))  )
//
//  def flatMap[B](f: A => MyGen[B]): MyGen[B] =
//    MyGen(
//      sample.flatMap(a => f(a).sample),
//      exhaustive.flatMap:
//        case None => unbounded
//        case Some(a) => f(a).exhaustive
//    )
//
//  def unit[A](a: => A): MyGen[A] = MyGen(State.unit(a), bounded(LazyList(a)))
//
//  def boolean: MyGen[Boolean] = MyGen(State(RNG.boolean), bounded(LazyList(true, false)))
//
//  // 根据返回值知道MyGen其实是一个State，所以要得到一个MyGen，就干脆返回一个State对象就可以了？最方便的方式是用State.apply的方法
//  def choose(start: Int, stopExclusive: Int): MyGen[Int] = MyGen(
//    State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)),
//    bounded(LazyList.from(start).take(stopExclusive - start))
//  )
//
//  /* This implementation is rather tricky, but almost impossible to get wrong
//   * if you follow the types. It relies on several helper functions (see below).
//   */
//  def listOfN[A](n: Int, g: MyGen[A]): MyGen[List[A]] =
//    MyGen(
//      State.sequence(List.fill(n)(g.sample)),
//      cartesian(LazyList.continually(g.exhaustive).take(n)).map(l => sequenceOption(l.toList))
//    )
//
//  /* `cartesian` generates all possible combinations of a `LazyList[LazyList[A]]`. For instance:
//   *
//   *    cartesian(LazyList(LazyList(1,2), LazyList(3), LazyList(4,5))) ==
//   *    LazyList(LazyList(1,3,4), LazyList(1,3,5), LazyList(2,3,4), LazyList(2,3,5))
//  */
//  def cartesian[A](s: LazyList[LazyList[A]]): LazyList[LazyList[A]] =
//    s.foldRight(LazyList(LazyList[A]()))((hs, ts) => map2LazyList(hs, ts)(LazyList.cons(_, _)))
//
//  /* This is not the same as `zipWith`, a function we've implemented before.
//   * We are generating all (A,B) combinations and using each to produce a `C`.
//   * This implementation desugars to sa.flatMap(a => sb.map(b => f(a,b))).
//   */
//  def map2LazyList[A, B, C](sa: LazyList[A], sb: => LazyList[B])(f: (A, => B) => C): LazyList[C] =
//    for {
//      a <- sa
//      b <- sb
//    } yield f(a, b)
//
//  /* `map2Option` and `map2LazyList`. Notice the duplication! */
//  def map2Option[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
//    for {
//      a <- oa
//      b <- ob
//    } yield f(a, b)
//
//  /* This is a function we've implemented before. Unfortunately, it does not
//   * exist in the standard library. This implementation is uses a foldLeft,
//   * followed by a reverse, which is equivalent to a foldRight, but does not
//   * use any stack space.
//   */
//  def sequenceOption[A](o: List[Option[A]]): Option[List[A]] =
//    o.foldLeft[Option[List[A]]](Some(List()))(
//      (t, h) => map2Option(h, t)(_ :: _)).map(_.reverse)
//
//  def double: MyGen[Double] = MyGen(State(RNG.double), unbounded)
//
//  def int: MyGen[Int] = MyGen(State(RNG.int), unbounded)
//}
//
//opaque type MySGen[/*+*/A] = Int => MyGen[A]
//
//object MyGen:
//  type Domain[+A] = LazyList[Option[A]]
//
//  def bounded[A](a: LazyList[A]): Domain[A] = a.map(Some(_))
//
//  def unbounded: Domain[Nothing] = LazyList(None)
//
//
//
//
//
//
//  extension[A] (self: MyGen[A]) def listOfN(size: Int): MyGen[List[A]] =
//    self.listOfN0(size)
//
//  extension[A] (self: MyGen[A]) def listOfN(size: MyGen[Int]): MyGen[List[A]] =
//    size.flatMap(self.listOfN0)
//
//  def union[A](g1: MyGen[A], g2: MyGen[A]): MyGen[A] =
//    boolean.flatMap(b => if b then g1 else g2)
//
//  def weighted[A](g1: (MyGen[A], Double), g2: (MyGen[A], Double)): MyGen[A] =
//    val (gen1, p1) = g1
//    val (gen2, p2) = g2
//    val g1Threshold = p1 / (p1 + p2)
//    State(RNG.double).flatMap(d => if d <= g1Threshold then gen1 else gen2)
//
//  extension[A] (self: MyGen[A]) def unsized: MySGen[A] = _ => self
//
//  extension[A] (self: MyGen[A]) def list: MySGen[List[A]] = n =>
//    self.listOfN0(n)
//
//  extension[A] (self: MyGen[A]) def nonEmptyList: MySGen[List[A]] =
//    n => self.listOfN(n.max(1))
//
//object MySGen {
//  import MyGen.*
//  extension [A](self: MySGen[A]) def map[B](f: A => B): MySGen[B] = n =>
//    self(n).map(f)
//
//  extension [A](self: MySGen[A]) def flatMap[B](f: A => MySGen[B]): MySGen[B] = n =>
//    self(n).flatMap(a => f(a)(n))
//}