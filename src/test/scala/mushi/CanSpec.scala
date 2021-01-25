package mushi

import cats.instances.int._
import cats.instances.string._
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.kernel.laws.discipline._
import org.scalacheck.{Arbitrary, Gen}
import org.typelevel.discipline.Laws

import munit.{FunSuite, ScalaCheckSuite, DisciplineSuite}

class CanSpec extends FunSuite with ScalaCheckSuite with DisciplineSuite {
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
  checkAll("Can.BitraverseLaws", BitraverseTests[Can].bitraverse[Option, Int, String, Long, Int, String, Long])
}
