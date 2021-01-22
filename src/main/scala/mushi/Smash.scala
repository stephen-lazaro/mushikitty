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
final case class Smash[A, B](value: Option[(A, B)])
object Smash {
  implicit val bifunctor: Bifunctor[Smash] = new Bifunctor[Smash] {
    def bimap[A, B, C, D](fac: Smash[A, B])(f: A => C, g: B => D): Smash[C, D] =
      Smash(fac.value.map(_.bimap(f, g)))
  }
  implicit val bifoldable: Bifoldable[Smash] = new Bifoldable[Smash] {

  }
  implicit def monoid[A, B](monoid: Monoid[A]): Monad[Smash[A, *]] = ???

  implicit def functor[A]: Functor[Smash[A, *]] = new Functor[Smash[A, *]] {
    def map[C, B](fa: Smash[A, C])(f: C => B): Smash[A, B] =
      Smash(fa.value.map(_.map(f)))
  }
  implicit def eq[A: Eq, B: Eq]: Eq[Smash[A, B]] = Eq.by(_.value)
}
