package mushi

import cats.{Bifoldable, Bifunctor, Eq, Functor, Monad, Monoid}
import cats.instances.option._
import cats.instances.tuple._
import cats.syntax.functor._
import cats.syntax.bifunctor._

/**
 * You can think of this as the Iff to
 * Ior and Either (Xor), if you want to.
 *
 * Either no value is present, or both are.
 *
 * This can be useful when enforcing the copresence
 * of values through various manipulations without
 * losing that information.
 */
sealed trait Smash[+A, +B]
object Smash {
  final case class Present[A, B](l: A, r: B) extends Smash[A, B]
  final case object Unaccounted extends Smash[Nothing, Nothing]

  def apply[A, B](value: Option[(A, B)]): Smash[A, B] =
    value.map((Present.apply[A, B] _).tupled).getOrElse(Unaccounted)

  /**
   * Embed our pointed product in the unpointed category.
   * i.e. give us the representation in terms of Option
   */
  def reify[A, B](self: Smash[A, B]): Option[(A, B)] = self match {
    case Present(l, r) => Some((l, r))
    case Unaccounted => None
  }

  implicit val bifunctor: Bifunctor[Smash] = new Bifunctor[Smash] {
    def bimap[A, B, C, D](fac: Smash[A, B])(f: A => C, g: B => D): Smash[C, D] =
      Smash(reify(fac).map(_.bimap(f, g)))
  }
  implicit val bifoldable: Bifoldable[Smash] = new Bifoldable[Smash] {
    def bifoldLeft[A, B, C](fab: mushi.Smash[A,B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
      fab match {
        case Present(a, b) => g(f(c, a), b)
        case Unaccounted => c
      }
    def bifoldRight[A, B, C](fab: mushi.Smash[A,B], c: cats.Eval[C])(f: (A, cats.Eval[C]) => cats.Eval[C], g: (B, cats.Eval[C]) => cats.Eval[C]): cats.Eval[C] =
      fab match {
        case Present(a, b) => g(b, f(a, c))
        case Unaccounted => c
      }
  }
  implicit def monad[A](implicit monoid: Monoid[A]): Monad[Smash[A, *]] = new Monad[Smash[A, *]] {
    def pure[C](value: C): Smash[A, C] = Present(monoid.empty, value)
    def flatMap[C, B](fac: Smash[A, C])(f: C => Smash[A, B]): Smash[A, B] =
      fac match {
        case Present(a, c) => f(c) match {
          case Present(a1, c2) => Present(monoid.combine(a, a1), c2)
          case Unaccounted => Unaccounted
        }
        case Unaccounted => Unaccounted
      }
    def tailRecM[C, B](c: C)(f: C => Smash[A, Either[C, B]]): Smash[A, B] =
        f(c) match {
          case Present(a, Right(b)) => Present(a, b)
          case Present(_, Left(c1)) => tailRecM(c1)(f)
          case Unaccounted => Unaccounted
        }
  }

  implicit def functor[A]: Functor[Smash[A, *]] = new Functor[Smash[A, *]] {
    def map[C, B](fa: Smash[A, C])(f: C => B): Smash[A, B] =
      Smash(reify(fa).map(_.map(f)))
  }
  implicit def eq[A: Eq, B: Eq]: Eq[Smash[A, B]] = Eq.by(reify)
}
