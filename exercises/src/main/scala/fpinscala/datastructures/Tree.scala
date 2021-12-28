package fpinscala.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_) => 0
    case Branch(l,r) => l.depth.max(r.depth) + 1

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B,B) => B): B = this match
    case Leaf(a) => f(a)
    case Branch(l,r) => g(l.fold(f,g),r.fold(f,g))
  
  def sizeViaFold: Int = fold(_ => 1, (x,y) => x+y+1)
  
  def depthViaFold: Int = fold(_ => 0, (x,y) => x.max(y) + 1)
  
  def mapViaFold[B](f: A => B): Tree[B] = fold(a => Leaf(f(a)), (l,r) => Branch(l,r))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Option[Int] = t match {
    case Leaf(x) => if x>0 then Some(x) else None
    case Branch(l,r) => l.firstPositive orElse r.firstPositive
  }

  extension (t: Tree[Int]) def maximum: Int = t match {
    case Leaf(x) => x
    case Branch(l,r) => l.maximum.max(r.maximum)
  }

  extension (t: Tree[Int]) def maximumViaFold: Int = ???

  @main def see =
    val t = Branch(
      Branch(Leaf(0),Leaf(-1)),
      Branch(Leaf(10),Leaf(2))
    )
    println(t.firstPositive)