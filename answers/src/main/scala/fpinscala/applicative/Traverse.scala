package fpinscala
package applicative

import monads.Functor
import state.State
import monoids.{Monoid, Foldable}
import Applicative.Const

trait Traverse[F[_]] extends Functor[F] with Foldable[F]:
  self =>

  extension [A](fa: F[A])
    def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] =
      fa.map(f).sequence

  extension [G[_]: Applicative, A](fga: F[G[A]])
    def sequence: G[F[A]] =
      fga.traverse(ga => ga)

  type Id[A] = A
  object Id:
    given idMonad: Monad[Id] with
      def unit[A](a: => A) = a
      extension [A](a: A)
        override def flatMap[B](f: A => B): B = f(a)

  extension [A](fa: F[A])
    def map[B](f: A => B): F[B] =
      fa.traverse[Id, B](f)(using Id.idMonad)

    override def foldMap[B: Monoid](f: A => B): B =
      fa.traverse[Const[B, _], Nothing](f)

    override def foldLeft[B](acc: B)(f: (B, A) => B): B =
      fa.mapAccum(acc)((a, b) => ((), f(b, a)))(1)

    override def toList: List[A] =
      fa.mapAccum(List[A]())((a, s) => ((), a :: s))(1).reverse

    def traverseS[S, B](f: A => State[S, B]): State[S, F[B]] =
      fa.traverse[State[S, _], B](f)(using Monad.stateMonad)

    def zipWithIndex_ : F[(A, Int)] =
      fa.traverseS(a => 
        for
          i <- State.get[Int]
          _ <- State.set(i + 1)
        yield (a, i)
      ).run(0)(0)

    def toList_ : List[A] =
      fa.traverseS(a => 
        for
          as <- State.get[List[A]] // Get the current state, the accumulated list.
          _  <- State.set(a :: as) // Add the current element and set the new list as the new state.
        yield ()
      ).run(Nil)(1).reverse

    def mapAccum[S, B](s: S)(f: (A, S) => (B, S)): (F[B], S) =
      fa.traverseS(a => 
        for
          s1 <- State.get[S]
          (b, s2) = f(a, s1)
          _  <- State.set(s2)
        yield b
      ).run(s)

    def zipWithIndex: F[(A, Int)] =
      fa.mapAccum(0)((a, s) => ((a, s), s + 1))(0)

    def reverse: F[A] =
      fa.mapAccum(fa.toList.reverse)((_, as) => (as.head, as.tail))(0)

    def zip[B](fb: F[B]): F[(A, B)] =
      fa.mapAccum(fb.toList) {
        case (a, Nil) => sys.error("zip: Incompatible shapes.")
        case (a, b :: bs) => ((a, b), bs)
      }(0)

    def zipL[B](fb: F[B]): F[(A, Option[B])] =
      fa.mapAccum(fb.toList) {
        case (a, Nil) => ((a, None), Nil)
        case (a, b :: bs) => ((a, Some(b): Option[B]), bs)
      }(0)

    def zipR[B](fb: F[B]): F[(Option[A], B)] =
      fb.mapAccum(fa.toList) {
        case (b, Nil) => ((None, b), Nil)
        case (b, a :: as) => ((Some(a): Option[A], b), as)
      }(0)

    def fuse[M[_]: Applicative, N[_]: Applicative, B](f: A => M[B], g: A => N[B]): (M[F[B]], N[F[B]]) =
      val m = summon[Applicative[M]]
      val n = summon[Applicative[N]]
      fa.traverse[[X] =>> (M[X], N[X]), B](a => (f(a), g(a)))(using m.product(n))

  def compose[G[_]: Traverse]: Traverse[[X] =>> F[G[X]]] = new:
    extension [A](fa: F[G[A]])
      override def traverse[H[_]: Applicative, B](f: A => H[B]): H[F[G[B]]] =
        self.traverse(fa)(ga => ga.traverse(f))

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse:
  given listTraverse: Traverse[List] with
    extension [A](as: List[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[List[B]] =
        val g = summon[Applicative[G]]
        as.foldRight(g.unit(List[B]()))((a, acc) => f(a).map2(acc)(_ :: _))

  given optionTraverse: Traverse[Option] with
    extension [A](oa: Option[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Option[B]] =
        oa match
          case Some(a) => f(a).map(Some(_))
          case None    => summon[Applicative[G]].unit(None)

  given treeTraverse: Traverse[Tree] = new:
    extension [A](ta: Tree[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Tree[B]] =
        f(ta.head).map2(ta.tail.traverse(a => a.traverse(f)))(Tree(_, _))

  // An example of a Foldable that is not a functor
  case class Iteration[A](a: A, f: A => A, n: Int):
    def foldMap[B](g: A => B)(using m: Monoid[B]): B =
      def iterate(n: Int, b: B, c: A): B =
        if n <= 0 then b else iterate(n - 1, g(c), f(a))
      iterate(n, m.empty, a)