package mushi

import cats.{Bifunctor, Functor, Eq, Monad, Order, Semigroup}
import cats.data.{Ior, NonEmptyVector}
import cats.instances.vector._
import cats.syntax.bifunctor._
import cats.syntax.foldable._
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
      fac match {
        case Lid(l, r) => Lid(f(l), g(r))
        case RimLeft(l) => RimLeft(f(l))
        case RimRight(r) => RimRight(g(r))
        case Base => Base
      }
  }

  implicit def functor[A]: Functor[Can[A, *]] = new Functor[Can[A, *]] {
    def map[C, B](fac: Can[A, C])(f: C => B): Can[A, B] =
      fac match {
        case Lid(l, r) => Lid(l, f(r))
        case RimLeft(l) => RimLeft(l)
        case RimRight(r) => RimRight(f(r))
        case Base => Base
      }
  }

  implicit def monad[A: Semigroup]: Monad[Can[A, *]] = new Monad[Can[A, *]] {
    def pure[B](b: B): Can[A, B] = RimRight(b)
    def flatMap[C, B](fac: Can[A, C])(f: C => Can[A, B]): Can[A, B] =
      fac match {
        case Lid(l, r) => f(r) match {
          case Lid(l2, r2) => Lid(Semigroup[A].combine(l, l2), r2)
          case RimLeft(l2) => RimLeft(Semigroup[A].combine(l, l2))
          case RimRight(r2) => Lid(l, r2)
          case Base => Base
        }
        case RimLeft(l) => RimLeft(l)
        case RimRight(r) => f(r)
        case Base => Base
      }
    def tailRecM[C, B](c: C)(f: C => Can[A, Either[C, B]]): Can[A, B] = {
      def go(acc: Vector[A], c: C): (Vector[A], Can[A, B]) = f(c) match {
        case Lid(l1, Right(b)) => (acc, Lid(l1, b))
        case Lid(l1, Left(c2)) => go(acc :+ l1, c2)
        case RimLeft(l) => (acc, RimLeft(l))
        case RimRight(Right(b)) => (acc, RimRight(b))
        case RimRight(Left(c2)) => go(acc, c2)
        case Base => (acc, Base)
      }

      go(Vector.empty, c) match {
        case (v, Lid(l, b)) =>
          if (v.isEmpty) Lid(l, b)
          else Lid(NonEmptyVector.fromVectorUnsafe(v :+ l).reduce, b)
        case (v, RimRight(b)) =>
          if (v.isEmpty) RimRight(b)
          else Lid(NonEmptyVector.fromVectorUnsafe(v).reduce, b)
        case (v, RimLeft(l)) => RimLeft(NonEmptyVector.fromVectorUnsafe(v :+ l).reduce)
        case (_, Base) => Base
      }
    }
  }


  implicit def eq[A: Eq, B: Eq]: Eq[Can[A, B]] = Eq.by(reify)
  implicit def ord[A: Order, B: Order]: Order[Can[A, B]] = Order.by(reify)
}
