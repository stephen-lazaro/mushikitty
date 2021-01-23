package mushi

import cats.laws.discipline._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.FlatSpecDiscipline

class CanSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with FlatSpecDiscipline {
  implicit def arbCan[A: Arbitrary, B: Arbitrary]: Arbitrary[Can[A, B]] =
    Arbitrary(
      for {
        a <- Gen option Arbitrary.arbitrary[A]
        b <- Gen option Arbitrary.arbitrary[B]
      } yield (a, b) match {
        case (Some(a), Some(b)) => Can.Lid(a, b)
        case (Some(a), None) => Can.RimLeft(a)
        case (None, Some(b)) => Can.RimRight(b)
        case (None, None) => Can.Base
      }
    )
  checkAll("Can.FunctorLaws", FunctorTests[Can[Int, *]].functor[Int, Int, String])
  checkAll("Can.MonadLaws", MonadTests[Can[Int, *]].monad[Int, Int, String])
  checkAll("Can.BifunctorLaws", BifunctorTests[Can].bifunctor[Int, Int, Int, String, String, String])
  //checkAll("Can.BifoldableLaws", BifoldableTests[Can].bifoldable[Int, String, Long])
  //checkAll("Can.BitraverseLaws", BitraverseTests[Can].bitraverse[Option, Int, String, Long, Int, String, Long])
}
