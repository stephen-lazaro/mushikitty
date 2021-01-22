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

  }
  implicit def monoid[A, B](monoid: Monoid[A]): Monad[Smash[A, *]] = ???

  implicit def functor[A]: Functor[Smash[A, *]] = new Functor[Smash[A, *]] {
    def map[C, B](fa: Smash[A, C])(f: C => B): Smash[A, B] =
      Smash(reify(fa).map(_.map(f)))
  }
  implicit def eq[A: Eq, B: Eq]: Eq[Smash[A, B]] = Eq.by(reify)
}
