package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.*

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toListRecursive: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRecursive
  }

  def toListTailRecButReverse: List[A] = {
    @tailrec
    def go(lazyList: LazyList[A], acc: List[A]): List[A] =
      lazyList match
        case Cons(h, t) => go(t(), h() :: acc)
        case Empty => acc.reverse

    go(this, List.empty[A])
  }

  def toListTailRecClean: List[A] = {
    val buf: ListBuffer[A] = new collection.mutable.ListBuffer[A]

    @tailrec
    def go(lazyList: LazyList[A]): List[A] =
      lazyList match
        case Cons(h, t) => buf += h(); go(t())
        case Empty => buf.toList

    go(this)
  }

  def toList: List[A] = toListTailRecClean

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), Empty)
    case _ => Empty

  def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this

  /* foldRight里有递归了，takeWhile就肯定不用再写一遍递归 */
  def takeWhile(p: A => Boolean): LazyList[A] = {
    /*this match
        case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
        case _ => Empty*/
    foldRight(LazyList.empty){ (a, b) => if p(a) then cons(a, b) else empty}
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true){(a, b) => p(a) && b}
  }

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): LazyList[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): LazyList[A] = {
    foldRight(empty[A])((a, b) => if p(a) then cons(a, b) else b)
  }

  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this):
      case Cons(h, t) => Some(f(h()), t())
      case _ => None

  def takeViaUnfold(num: Int): LazyList[A] =
    unfold((this, num)):
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n - 1))
      case _ => None

  def takeWhileViaUnfold(f: A => Boolean): LazyList[A] =
    unfold(this):
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _ => None

  def zipWith[B,C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, that)):
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)):
      case (Cons(h1, t1), Cons(h2, t2)) => Some(Some(h1()) -> Some(h2()), t1() -> t2())
      case (Cons(h1, t1), _) => Some(Some(h1()) -> None, t1() -> empty[B])
      case (_, Cons(h2, t2)) => Some(None -> Some(h2()), empty[A] -> t2())
      case _ => None

  def startsWith[B](s: LazyList[B]): Boolean = ???
//    foldRight(true)((a, b) => )

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = LazyList.cons(a, continually(a))

  def from(n: Int): LazyList[Int] = LazyList.cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] = {
    def go(current: Int, next: Int): LazyList[Int] =
      cons(current, go(next, current + next))
    go(0, 1)
  }

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _ => empty

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold[Int, (Int, Int)]((0, 1)):
      case (current, next) => Some((current, (next, current + next)))

  def fromViaUnfold(n: Int): LazyList[Int] = unfold[Int, Int](n)(n => Some(n, n + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold[A, Unit](())(Some(a, _))

  lazy val onesViaUnfold: LazyList[Int] = continuallyViaUnfold(1)

  @main def testFunc(): Unit = {
    println(LazyList(1, 2, 3).take(2).toListRecursive)
    println(LazyList(1, 2, 3).takeWhile(_ < 3).toListRecursive)
  }