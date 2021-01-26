package mushi

import cats.instances.int._
import cats.instances.string._
import cats.laws.discipline._
import cats.kernel.laws.discipline._
import cats.syntax.eq._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Prop._

import munit.{FunSuite, ScalaCheckSuite, DisciplineSuite}

class WedgeSpec extends FunSuite with ScalaCheckSuite with DisciplineSuite {
  implicit def arbWedge[A: Arbitrary, B: Arbitrary]: Arbitrary[Wedge[A, B]] =
    Arbitrary(
      for {
        a <- Gen option Arbitrary.arbitrary[A]
        b <- if (a.isEmpty) Gen option Arbitrary.arbitrary[B] else Gen const None
     } yield
       if (a.isDefined) Wedge.WLeft(a.get)
       else if (b.isDefined) Wedge.WRight(b.get)
       else Wedge.WEmpty
    )

  implicit def cogenWedge[A: Cogen, B: Cogen]: Cogen[Wedge[A, B]] =
    Cogen[Option[Either[A, B]]].contramap(Wedge.reify)

  checkAll("Wedge.EqLaws", EqTests[Wedge[Int, Long]].eqv)
  checkAll("Wedge.OrderLaws", OrderTests[Wedge[Int, Long]].order)
  checkAll("Wedge.FunctorLaws", FunctorTests[Wedge[Int, *]].functor[Int, Int, String])
  checkAll("Wedge.MonadLaws", MonadTests[Wedge[Int, *]].monad[Int, Int, String])
  checkAll("Wedge.BitraverseLaws", BitraverseTests[Wedge].bitraverse[Option, Int, String, Long, Int, String, Long])

  property("Wedge.swap symmetric") { forAll { (wedge: Wedge[Int, Int]) => wedge.swap.swap === wedge} }
  property("Wedge.assocRight andThen Wedge.assocLeft is identity")(forAll((wedge: Wedge[Wedge[Int, Int], Int]) => Wedge.assocLeft(Wedge.assocRight(wedge)) === wedge))
  property("Wedge.assocLeft andThen Wedge.assocRight is identity")(forAll((wedge: Wedge[Int, Wedge[Int, Int]]) => Wedge.assocRight(Wedge.assocLeft(wedge)) === wedge))
}
