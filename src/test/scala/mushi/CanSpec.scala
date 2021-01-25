package mushi

import cats.data.Ior
import cats.instances.int._
import cats.instances.string._
import cats.syntax.eq._
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.kernel.laws.discipline._
import org.scalacheck._
import org.scalacheck.Prop._
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

  implicit def cogenCan[A: Cogen, B: Cogen]: Cogen[Can[A, B]] =
    Cogen[Option[Ior[A, B]]].contramap(Can.reify)

  checkAll("Can.EqLaws", EqTests[Can[Int, Long]].eqv)
  checkAll("Can.OrderLaws", OrderTests[Can[Int, Long]].order)
  checkAll("Can.FunctorLaws", FunctorTests[Can[Int, *]].functor[Int, Int, String])
  checkAll("Can.MonadLaws", MonadTests[Can[Int, *]].monad[Int, Int, String])
  checkAll("Can.BitraverseLaws", BitraverseTests[Can].bitraverse[Option, Int, String, Long, Int, String, Long])

  property("Can.swap symmetric") { forAll { (can: Can[Int, Int]) => can.swap.swap === can } }
}
