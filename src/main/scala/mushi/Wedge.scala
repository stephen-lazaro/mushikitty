package mushi

import cats.{Bifunctor, Eq, Order}
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
    case WLeft(l) => Some(Left(l))
    case WRight(r) => Some(Right(r))
    case WEmpty => None
  }

  implicit val bifunctor: Bifunctor[Wedge] = new Bifunctor[Wedge] {
    def bimap[A, B, C, D](fac: Wedge[A, B])(f: A => C, g: B => D): Wedge[C, D] =
      Wedge(reify(fac).map(_.bimap(f, g)))
  }

  implicit def eq[A: Eq, B: Eq]: Eq[Wedge[A, B]] = Eq.by(reify)
  implicit def ord[A: Order, B: Order]: Order[Wedge[A, B]] = Order.by(reify)
}
