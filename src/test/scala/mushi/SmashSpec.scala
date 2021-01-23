package mushi

import cats.laws.discipline._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.FlatSpecDiscipline

class SmashSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with FlatSpecDiscipline {
  implicit def arbSmash[A: Arbitrary, B: Arbitrary]: Arbitrary[Smash[A, B]] =
    Arbitrary(
      for {
        a <- Gen option Arbitrary.arbitrary[A]
        b <- if (a.isDefined) Gen some Arbitrary.arbitrary[B] else Gen const None
      } yield
        if (a.isDefined) Smash.Present(a.get, b.get)
        else Smash.Unaccounted
    )
  checkAll("Smash.FunctorLaws", FunctorTests[Smash[Int, *]].functor[Int, Int, String])
  checkAll("Smash.MonadLaws", MonadTests[Smash[Int, *]].monad[Int, Int, String])
  checkAll("Smash.BifunctorLaws", BifunctorTests[Smash].bifunctor[Int, Int, Int, String, String, String])
  checkAll("Smash.BifoldableLaws", BifoldableTests[Smash].bifoldable[Int, String, Long])
  checkAll("Smash.BitraverseLaws", BitraverseTests[Smash].bitraverse[Option, Int, String, Long, Int, String, Long])
}
