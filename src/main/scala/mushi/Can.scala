package mushi

import cats.Bifunctor
import cats.data.Ior
import cats.syntax.bifunctor._

sealed trait Can[+A, +B]

object Can {
  case class Lid[A, B](l: A, r: B) extends Can[A, B]
  case class RimLeft[A](value: A) extends Can[A, Nothing]
  case class RimRight[B](value: B) extends Can[Nothing, B]
  case object Base extends Can[Nothing, Nothing]

  /**
   * Embed our pointed pointed product in the unpointed category.
   * i.e. give us the representation in terms of Option
   */
  def reify[A, B](self: Can[A, B]): Option[Ior[A, B]] = self match {
    case Lid(l, r) => Some(Ior.Both(l, r))
    case RimLeft(l) => Some(Ior.Left(l))
    case RimRight(r) => Some(Ior.Right(r))
    case Base => None
  }

  def apply[A, B](value: Option[Ior[A, B]]): Can[A, B] =
    value.map({
      case Ior.Both(l ,r) => Lid(l, r)
      case Ior.Left(l) => RimLeft(l)
      case Ior.Right(r) => RimRight(r)
    }).getOrElse(Base)

  implicit val bifunctor: Bifunctor[Can] = new Bifunctor[Can] {
    def bimap[A, B, C, D](fac: Can[A, B])(f: A => C, g: B => D): Can[C, D] =
      Can(reify(fac).map(_.bimap(f, g)))
  }
}
