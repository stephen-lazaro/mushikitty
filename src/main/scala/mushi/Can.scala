package mushi

import cats.{Bifunctor, Bitraverse, Foldable, Functor, Eq, Monad, Order, Semigroup}
import cats.data.{Ior, NonEmptyVector}
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._

sealed trait Can[+A, +B]
object Can {
  case class Lid[A, B](l: A, r: B) extends Can[A, B]
  case class RimLeft[A](value: A) extends Can[A, Nothing]
  case class RimRight[B](value: B) extends Can[Nothing, B]
  case object Base extends Can[Nothing, Nothing]

  def isLid[A, B](value: Can[A, B]): Boolean =
    fold(value)(false, _ => false, _ => false, (_, _) => true)
  def isRimLeft[A, B](value: Can[A, B]): Boolean =
    fold(value)(false, _ => true, _ => false, (_, _) => false)
  def isRimRight[A, B](value: Can[A, B]): Boolean =
    fold(value)(false, _ => false, _ => true, (_, _) => false)
  def isBase[A, B](value: Can[A, B]): Boolean =
    fold(value)(true, _ => false, _ => false, (_, _) => false)

  def zip[F[_]: Foldable: Functor, A, B](value: Can[F[A], F[B]]): List[Can[A, B]] = value match {
    case Lid(l, r) => (0L to Math.max(l.size, r.size)).map((i: Long) =>
          (l.get(i), r.get(i)) match {
            case (Some(a), Some(b)) => Lid(a, b)
            case (Some(a), None) => RimLeft(a)
            case (None, Some(b)) => RimRight(b)
            case (None, None) => Base
          }
        ).toList
    case RimLeft(l) => l.map(RimLeft.apply).toList
    case RimRight(r) => r.map(RimRight.apply).toList
    case Base => List.empty // bit odd, should this be List(Base)?
  }

  def fold[A, B, C](value: Can[A, B])(default: C, f: A => C, g: B => C, h: (A, B) => C): C =
    value match {
      case Lid(a, b) => h(a, b)
      case RimLeft(a) => f(a)
      case RimRight(b) => g(b)
      case Base => default
    }

  def curry[A, B, C](value: Can[A, B] => Option[C]): Option[A] => Option[B] => Option[C] = {
    case Some(a) => {
      case Some(b) => value(Lid(a, b))
      case None => value(RimLeft(a))
    }
    case None =>  {
      case Some(b) => value(RimRight(b))
      case None => value(Base)
    }
  }

  def uncurry[A, B, C](f: Option[A] => Option[B] => Option[C]): Can[A, B] => Option[C] = {
    case Lid(a, b) => f(Option(a))(Option(b))
    case RimLeft(a) => f(Option(a))(None)
    case RimRight(b) => f(None)(Option(b))
    case Base => f(None)(None)
  }

  def swap[A, B](can: Can[A, B]): Can[B, A] =
    fold(can)(Base, RimRight.apply[A], RimLeft.apply[B], (a, b) => Lid[B, A](b, a))
  def assocRight[A, B, C](value: Can[Can[A, B], C]): Can[A, Can[B, C]] = value match {
    case Lid(Lid(a, b), c) => Lid(a, Lid(b, c))
    case Lid(RimLeft(a), c) => Lid(a, RimRight(c))
    case Lid(RimRight(b), c) => RimRight(Lid(b, c))
    case Lid(Base, c) => RimRight(RimRight(c))
    case RimLeft(Lid(a, b)) => Lid(a, RimLeft(b))
    case RimLeft(RimLeft(a)) => RimLeft(a)
    case RimLeft(RimRight(b)) => RimRight(RimLeft(b))
    case RimLeft(Base) => RimRight(Base)
    case RimRight(c) => RimRight(RimRight(c))
    case Base => Base
  }
  def assocLeft[A, B, C](value: Can[A, Can[B, C]]): Can[Can[A, B], C] = ???

  /**
   * Embed our pointed pointed product in the unpointed category.
   * i.e. give us the representation in terms of Option
   */
  def reify[A, B](self: Can[A, B]): Option[Ior[A, B]] =
    fold(self)(
      None,
      Some.apply[Ior[A, B]] _ compose Ior.Left.apply[A],
      Some.apply[Ior[A, B]] _ compose Ior.Right.apply[B],
      (a, b) => Some(Ior.Both(a, b))
    )

  implicit class standardCanUtilityOps[A, B](value: Can[A, B]) {
    def reify = Can.reify(value)
    def swap = Can.swap(value)
    def fold[C](default: C, f: A => C, g: B => C, h: (A, B) => C) =
      Can.fold(value)(default, f, g, h)
    def isLid = Can.isLid(value)
    def isRimLeft = Can.isRimLeft(value)
    def isRimRight = Can.isRimRight(value)
    def isBase = Can.isBase(value)
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

  implicit val Bitraverse: Bitraverse[Can] = new Bitraverse[Can] {
    def bifoldLeft[A, B, C](fab: mushi.Can[A,B], c: C)(f: (C, A) => C, g: (C, B) => C): C = fab match {
      case Lid(a, b) => g(f(c, a), b)
      case RimRight(b) => g(c, b)
      case RimLeft(a) => f(c, a)
      case Base => c
    }
    def bifoldRight[A, B, C](fab: mushi.Can[A,B], c: cats.Eval[C])(f: (A, cats.Eval[C]) => cats.Eval[C], g: (B, cats.Eval[C]) => cats.Eval[C]): cats.Eval[C] = fab match {
      case Lid(a, b) => g(b, f(a, c))
      case RimRight(b) => g(b, c)
      case RimLeft(a) => f(a, c)
      case Base => c
    }
    def bitraverse[G[_], A, B, C, D](fab: mushi.Can[A,B])(f: A => G[C], g: B => G[D])(implicit evidence$1: cats.Applicative[G]): G[mushi.Can[C,D]] = fab match {
      case Lid(a, b) => (f(a), g(b)).mapN(Lid.apply[C, D])
      case RimRight(b) => g(b).map(RimRight.apply[D])
      case RimLeft(a) => f(a).map(RimLeft.apply[C])
      case Base => Base.pure[G].widen
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
