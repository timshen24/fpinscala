package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (l.depth max r.depth)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B,B) => B): B = this match
    case Leaf(v) => f(v)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))
  
  def sizeViaFold: Int = fold(_ => 1, _ + _)
  
  def depthViaFold: Int = fold(_ => 1, _ max _)
  
  def mapViaFold[B](f: A => B): Tree[B] = fold(a => Leaf(f(a)), Branch(_, _))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int = t match
    case Tree.Leaf(value) => value
    case Tree.Branch(left, right) =>
      val lpos = left.firstPositive
      if lpos > 0 then lpos else right.firstPositive

  extension (t: Tree[Int]) def maximum: Int = {
    t match
      case Tree.Leaf(value) => value
      case Tree.Branch(left, right) => left.maximum max right.maximum
  }

  extension (t: Tree[Int]) def maximumViaFold: Int = t.fold(a => a, _ max _)
