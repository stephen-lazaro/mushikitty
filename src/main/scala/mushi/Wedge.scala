package mushi

import cats.{Bifunctor, Eq, Functor, Monad, Order, Semigroup}
import cats.instances.either._
import cats.syntax.bifunctor._

sealed trait Wedge[+A, +B]
object Wedge {
  case class WLeft[A](value: A) extends Wedge[A, Nothing]
  case class WRight[B](value: B) extends Wedge[Nothing, B]
  case object WEmpty extends Wedge[Nothing, Nothing]

  def apply[A, B](value: Option[Either[A, B]]): Wedge[A, B] =
    value.map(_.fold(WLeft.apply[A], WRight.apply[B])).getOrElse(WEmpty)

  /**
   * Embed our pointed coproduct in the unpointed category.
   * i.e. give us the representation in terms of Option
   */
  def reify[A, B](self: Wedge[A, B]): Option[Either[A, B]] = self match {
    case WLeft(l)  => Some(Left(l))
    case WRight(r) => Some(Right(r))
    case WEmpty    => None
  }

  implicit val bifunctor: Bifunctor[Wedge] = new Bifunctor[Wedge] {
    def bimap[A, B, C, D](fac: Wedge[A, B])(f: A => C, g: B => D): Wedge[C, D] =
      Wedge(reify(fac).map(_.bimap(f, g)))
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
