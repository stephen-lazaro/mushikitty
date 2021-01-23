package mushi

import cats.laws.discipline._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.FlatSpecDiscipline

class WedgeSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with FlatSpecDiscipline {
//  implicit def arbWedge[A: Arbitrary, B: Arbitrary]: Arbitrary[Wedge[A, B]] =
//    Arbitrary(
//      for {
//        a <- Gen option Arbitrary.arbitrary[A]
//        b <- if (a.isDefined) Gen some Arbitrary.arbitrary[B] else Gen const None
//      } yield
//        if (a.isDefined) Wedge.Present(a.get, b.get)
//        else Wedge.Unaccounted
//    )
//  checkAll("Wedge.FunctorLaws", FunctorTests[Wedge[Int, *]].functor[Int, Int, String])
//  checkAll("Wedge.MonadLaws", MonadTests[Wedge[Int, *]].monad[Int, Int, String])
//  checkAll("Wedge.BifunctorLaws", BifunctorTests[Wedge].bifunctor[Int, Int, Int, String, String, String])
//  checkAll("Wedge.BifoldableLaws", BifoldableTests[Wedge].bifoldable[Int, String, Long])
//  checkAll("Wedge.BitraverseLaws", BitraverseTests[Wedge].bitraverse[Option, Int, String, Long, Int, String, Long])
}
