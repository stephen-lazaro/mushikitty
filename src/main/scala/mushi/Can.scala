package mushi

import cats.{Bifunctor, Functor, Monad, Semigroup}
import cats.data.Ior
import cats.syntax.bifunctor._
import cats.syntax.functor._

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

  implicit def functor[A]: Functor[Can[A, *]] = new Functor[Can[A, *]] {
    def map[C, B](fac: Can[A, C])(f: C => B): Can[A, B] =
      Can(reify(fac).map(_.map(f)))
  }

  implicit def monad[A: Semigroup]: Monad[Can[A, *]] = new Monad[Can[A, *]] {
    def pure[B](b: B): Can[A, B] = RimRight(b)
    def flatMap[C, B](fac: Can[A, C])(f: C => Can[A, B]): Can[A, B] =
      fac match {
        case Lid(l, r) => f(r) match {
          case Lid(l2, r2) => Lid(Semigroup[A].combine(l, l2), r2)
          case RimLeft(l2) => RimLeft(Semigroup[A].combine(l, l2))
          case RimRight(r2) => RimRight(r2)
          case Base => Base
        }
        case RimLeft(l) => RimLeft(l)
        case RimRight(r) => f(r)
        case Base => Base
      }
    def tailRecM[C, B](c: C)(f: C => Can[A, Either[C, B]]): Can[A, B] =
      f(c) match {
        case Lid(l1, Right(b)) => Lid(l1, b)
        case Lid(l1, Left(c2)) => tailRecM(c2)(f)
        case RimLeft(l) => RimLeft(l)
        case RimRight(Right(b)) => RimRight(b)
        case RimRight(Left(c2)) => tailRecM(c2)(f)
        case Base => Base
      }
  }
}
