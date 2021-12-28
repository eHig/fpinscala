package fpinscala.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(ds: List[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  val x = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]) =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]) =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_,t) => t
    case Nil => sys.error("tail of empty list")
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_,t) => Cons(h,t)
    case Nil => sys.error("setHead of empty list")
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if n<=0 then l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t,n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h,t) if f(h) => dropWhile(t,f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init on empty")
    case Cons(_,Nil) => Nil
    case Cons(x,t) => Cons(x,init(t))
    case _ => Nil
  }

  def length[A](l: List[A]): Int =
    foldRight(l,0, (_,agg) => agg+1)

  @tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B =
    l match {
      case Nil => acc
      case Cons(h,t) => foldLeft(t,f(acc,h),f)
    }

  def sumViaFoldLeft(ns: List[Int]) = foldLeft(ns,0,_+_)

  def productViaFoldLeft(ns: List[Double]) = foldLeft(ns,1.0,_*_)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l,0,(agg,_) =>agg+1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l,Nil:List[A],(rev,a) => Cons(a,rev) )

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r, Cons(_,_) )

  def foldRightViaFoldLeft1[A,B](l:List[A],acc:B,f:(A,B)=>B): B =
    foldLeft(reverse(l), acc, (b,a) => f(a,b))

  def foldRightViaFoldLeft2[A,B](l:List[A],acc:B,f:(A,B)=>B): B =
    val init:B => B = b => b
    val update: (B=>B,A)=>(B=>B) = (agg,a) => (b:B) => agg(f(a,b))// f(a,agg(b))
    val function:B=>B = foldLeft[A,B=>B](l,init, update)
    function(acc)


  def foldRightViaFoldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(as, (b: B) => b, (g, a) => b => g(f(a, b)))(acc)

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l,Nil:List[A],(next,acc)=>append(next,acc))

  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l,Nil:List[Int],(i,rem)=>Cons(i+1,rem))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l,Nil:List[String],(d,rem)=>Cons(d.toString,rem))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l,Nil:List[B], (a,rem)=> Cons(f(a),rem))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A], (a,acc)=> if(f(a)) then Cons(a,acc) else acc)

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, Nil:List[B], (a,acc)=> append(f(a),acc))

  def filterFromFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if(f(a)) then List(a) else Nil:List[A])


  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a,b) match {
      case (Cons(ha,ta),Cons(hb,tb)) => Cons(ha+hb, addPairwise(ta,tb))
      case (_, _) => Nil
    }

  // def zipWith - TODO determine signature

  def zipWith[A,B,C](as:List[A],bs:List[B],f:(A,B)=>C):List[C] = {
    (as,bs) match {
      case (Cons(ha,ta),Cons(hb,tb)) => Cons(f(ha,hb), zipWith(ta,tb,f))
      case _ => Nil
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    def startsWithSubsequence(x:List[A],y:List[A]):Boolean =
      (x,y) match {
        case (Cons(xh,xt), Cons(yh,yt)) if xh == yh => startsWithSubsequence(xt,yt)
        case (_, Nil) => true
        case _ => false
      }
    if startsWithSubsequence(sup,sub) then true
    else sup match {
      case Nil => false
      case Cons(h,t) => hasSubsequence(t,sub)
    }

  def mkString[A](l:List[A]) = {
    val commas = map(l)(x => s"${x}, ")
    "(" + foldRight(commas,"",_+_) + ")"
  }

object Blah:
  import List.*
  @main def test(): Unit = {
    testHasSubsequence
  }

  def testHasSubsequence = {
    val inputs = Seq(
      (List(1,2,3,4,5),List(1,2)),
      (List(1,2,3,4,5),List(2,3,4)),
      (List(1,2,3,4,5),List(1,2,3,4,5)),
      (List(1,2,3,4,5),List(5)),
      (List(1,2,3,4,5),List(1,2,3,4,5,6)),

      (List(1,2,3),List(6)),
      (List(1,2,3,4),List(1,5)),
      (List(1,2,3,4),List(3,4,5)),
    )

    inputs.foreach( i => {
      val res = hasSubsequence(i._1,i._2)
      println(s"${mkString(i._1)} - ${mkString(i._2)} => ${res}")
    })
  }

