package mushi

import cats.instances.int._
import cats.instances.string._
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.kernel.laws.discipline._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.typelevel.discipline.Laws

import munit.{FunSuite, ScalaCheckSuite, DisciplineSuite}

class SmashSpec extends FunSuite with ScalaCheckSuite with DisciplineSuite {
  implicit def arbSmash[A: Arbitrary, B: Arbitrary]: Arbitrary[Smash[A, B]] =
    Arbitrary(
      for {
        a <- Gen option Arbitrary.arbitrary[A]
        b <- if (a.isDefined) Gen some Arbitrary.arbitrary[B] else Gen const None
      } yield
        if (a.isDefined) Smash.Present(a.get, b.get)
        else Smash.Unaccounted
    )

  implicit def cogenSmash[A: Cogen, B: Cogen]: Cogen[Smash[A, B]] =
    Cogen[Option[(A, B)]].contramap(Smash.reify)

  checkAll("Smash.EqLaws", EqTests[Smash[Int, Long]].eqv)
  checkAll("Smash.OrderLaws", OrderTests[Smash[Int, Long]].order)
  checkAll("Smash.FunctorLaws", FunctorTests[Smash[Int, *]].functor[Int, Int, String])
  checkAll("Smash.MonadLaws", MonadTests[Smash[Int, *]].monad[Int, Int, String])
  checkAll("Smash.BitraverseLaws", BitraverseTests[Smash].bitraverse[Option, Int, String, Long, Int, String, Long])
}
