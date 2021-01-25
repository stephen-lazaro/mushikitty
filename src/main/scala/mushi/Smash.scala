package mushi

import cats.{Bitraverse, Eq, Functor, Monad, Monoid, Order}
import cats.instances.vector._
import cats.instances.option._
import cats.instances.tuple._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.foldable._

/**
 * You can think of this as the Iff to
 * Ior and Either (Xor), if you want to.
 *
 * Either no value is present, or both are.
 *
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
  def reify[A, B](self: Smash[A, B]): Option[(A, B)] =
    fold(self)(None, (a, b) => Option((a, b)))

  def fold[A, B, C](self: Smash[A, B])(default: C, f: (A, B) => C): C = self match {
    case Present(l, r) => f(l, r)
    case Unaccounted => default
  }

  def isPresent[A, B](self: Smash[A, B]): Boolean =
    fold(self)(false, (_, _) => true)
  def isUnaccounted[A, B](self: Smash[A, B]): Boolean =
    fold(self)(true, (_, _) => false)
  def swap[A, B](self: Smash[A, B]): Smash[B, A] =
    fold(self)(Unaccounted, (l, r) => Present(r, l))

  implicit class standardSmashUtilityOps[A, B](value: Smash[A, B]) {
    def reify = Smash.reify(value)
    def swap = Smash.swap(value)
    def fold[C](default: C, f: (A, B) => C): C =
      Smash.fold(value)(default, f)
    def isPresent: Boolean = Smash.isPresent(value)
    def isUnaccounted: Boolean = Smash.isUnaccounted(value)
  }

  implicit val bitraverse: Bitraverse[Smash] = new Bitraverse[Smash] {
    def bitraverse[G[_], A, B, C, D](fab: mushi.Smash[A,B])(f: A => G[C], g: B => G[D])(implicit evidence$1: cats.Applicative[G]): G[mushi.Smash[C,D]] =
      fab match {
        case Present(a, b) => (f(a), g(b)).mapN(Present.apply[C, D])
        case Unaccounted => Unaccounted.pure[G].widen
      }
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
    def tailRecM[C, B](c: C)(f: C => Smash[A, Either[C, B]]): Smash[A, B] = {
      def go(acc: Vector[A], c: C): (Vector[A], Smash[A, B]) =
        f(c) match {
          case Present(a, Right(b)) => (acc, Present(a, b))
          case Present(a, Left(c1)) => go(acc :+ a, c1)
          case Unaccounted => (acc, Unaccounted)
        }
      go(Vector.empty, c) match {
            case (v, Present(a, c)) => Present((v :+ a).combineAll, c)
            case (_, Unaccounted) => Unaccounted
          }
    }
  }

  implicit def functor[A]: Functor[Smash[A, *]] = new Functor[Smash[A, *]] {
    def map[C, B](fa: Smash[A, C])(f: C => B): Smash[A, B] =
      Smash(reify(fa).map(_.map(f)))
  }
  implicit def eq[A: Eq, B: Eq]: Eq[Smash[A, B]] = Eq.by(reify)
  implicit def ord[A: Order, B: Order]: Order[Smash[A, B]] = Order.by(reify)
}
