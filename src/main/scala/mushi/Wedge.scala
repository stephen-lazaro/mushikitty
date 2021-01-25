package mushi

import cats.{Bitraverse, Eq, Functor, Monad, Order, Semigroup}
import cats.instances.either._
import cats.syntax.applicative._
import cats.syntax.functor._

sealed trait Wedge[+A, +B]
object Wedge {
  case class WLeft[A](value: A) extends Wedge[A, Nothing]
  case class WRight[B](value: B) extends Wedge[Nothing, B]
  case object WEmpty extends Wedge[Nothing, Nothing]

  def apply[A, B](value: Option[Either[A, B]]): Wedge[A, B] =
    value.map(_.fold(WLeft.apply[A], WRight.apply[B])).getOrElse(WEmpty)

  def fold[A, B, C](value: Wedge[A, B])(default: C, f: A => C, g: B => C): C =
    value match {
      case WLeft(a) => f(a)
      case WRight(b) => g(b)
      case WEmpty => default
    }

  def isLeft[A, B](value: Wedge[A, B]): Boolean =
    fold(value)(false, _ => true, _ => false)
  def isRight[A, B](value: Wedge[A, B]): Boolean =
    fold(value)(false, _ => false, _ => true)
  def isEmpty[A, B](value: Wedge[A, B]): Boolean =
    fold(value)(true, _ => false, _ => false)

  def swap[A, B](value: Wedge[A, B]): Wedge[B, A] =
    fold(value)(WEmpty, WRight.apply, WLeft.apply)
  def assocRight[A, B, C](value: Wedge[Wedge[A, B], C]): Wedge[A, Wedge[B, C]] = value match {
    case WRight(c) => WRight(WRight(c))
    case WLeft(WRight(b)) => WRight(WLeft(b))
    case WLeft(WLeft(a)) => WLeft(a)
    case WLeft(WEmpty) => WRight(WEmpty)
    case WEmpty => WEmpty
  }
  def assocLeft[A, B, C](value: Wedge[A, Wedge[B, C]]): Wedge[Wedge[A, B], C] = ???

  /**
   * Embed our pointed coproduct in the unpointed category.
   * i.e. give us the representation in terms of Option
   */
  def reify[A, B](self: Wedge[A, B]): Option[Either[A, B]] =
    fold(self)(
      None,
      l => Some(Left(l)),
      r => Some(Right(r))
    )

  implicit class standardWedgeUtilityOps[A, B](value: Wedge[A, B]) {
    def reify = Wedge.reify(value)
    def swap = Wedge.swap(value)
    def fold[C](default: C, f: A => C, g: B => C): C =
      Wedge.fold(value)(default, f, g)
    def isLeft: Boolean = Wedge.isLeft(value)
    def isRight: Boolean = Wedge.isRight(value)
    def isEmpty: Boolean = Wedge.isEmpty(value)
  }

  implicit val bitraverse: Bitraverse[Wedge] = new Bitraverse[Wedge] {
    def bifoldLeft[A, B, C](fab: Wedge[A,B], c: C)(f: (C, A) => C, g: (C, B) => C): C = fab match {
      case WLeft(l) => f(c, l)
      case WRight(r) => g(c, r)
      case WEmpty => c
    }
    def bifoldRight[A, B, C](fab: Wedge[A,B], c: cats.Eval[C])(f: (A, cats.Eval[C]) => cats.Eval[C], g: (B, cats.Eval[C]) => cats.Eval[C]): cats.Eval[C] = fab match {
      case WLeft(l) => f(l, c)
      case WRight(r) => g(r, c)
      case WEmpty => c
    }
    def bitraverse[G[_], A, B, C, D](fab: Wedge[A,B])(f: A => G[C], g: B => G[D])(implicit evidence$1: cats.Applicative[G]): G[Wedge[C,D]] = fab match {
      case WLeft(l) => f(l).map(WLeft.apply)
      case WRight(r) => g(r).map(WRight.apply)
      case WEmpty => WEmpty.pure[G].widen
    }
  }

  implicit def functor[A]: Functor[Wedge[A, *]] = new Functor[Wedge[A, *]] {
    def map[C, B](fac: Wedge[A, C])(f: C => B): Wedge[A, B] = fac match {
      case WLeft(a)  => WLeft(a)
      case WRight(c) => WRight(f(c))
      case WEmpty    => WEmpty
    }
  }

  implicit def monad[A: Semigroup]: Monad[Wedge[A, *]] = new Monad[Wedge[A, *]] {
    def pure[C](c: C): Wedge[A, C] = WRight(c)
    def flatMap[C, B](fac: Wedge[A, C])(f: C => Wedge[A, B]): Wedge[A, B] = fac match {
      case WLeft(a)  => WLeft(a)
      case WRight(c) => f(c)
      case WEmpty    => WEmpty
    }

    def tailRecM[C, B](c: C)(f: C => Wedge[A, Either[C, B]]): Wedge[A, B] = f(c) match {
      case WRight(Right(b)) => WRight(b)
      case WRight(Left(c)) => tailRecM(c)(f)
      case WLeft(a) => WLeft(a)
      case WEmpty => WEmpty
    }
  }


  implicit def eq[A: Eq, B: Eq]: Eq[Wedge[A, B]] = Eq.by(reify)
  implicit def ord[A: Order, B: Order]: Order[Wedge[A, B]] = Order.by(reify)
}
