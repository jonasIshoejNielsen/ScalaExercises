// Advanced Programming, Andrzej Wasowski, IT University of Copenhagen
package adpro

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

class NodeSpec
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers
  with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
  with org.scalatest.prop.TableDrivenPropertyChecks {

  import Reduce._

  def genNode2[A: Arbitrary]: Gen[Node2[A]] =
    for {
      l <- arbitrary[A]
      r <- arbitrary[A]
    } yield Node2 (l, r)


  def genNode3[A: Arbitrary]: Gen[Node3[A]] =
    for {
      l <- arbitrary[A]
      m <- arbitrary[A]
      r <- arbitrary[A]
    } yield Node3 (l, m, r)


  def genNode[A: Arbitrary]: Gen[Node[A]] =
    for {
      c <- Gen.oneOf (true, false)
      n <- if (c) genNode2[A] else genNode3[A]
    } yield n

  implicit def arbNode[A: Arbitrary]: Arbitrary[Node[A]] =
    Arbitrary { genNode[A] }

  "Exercise 10 (Reduce[Node])" - {

    "Reduce[Node] reducers collapse for monoids Int+" in {
      implicit val monoid = adpro.Monoid.intAddition
      reduce[Node].Laws.reducersEquivalentForMonoids[Int]
    }

    "Reduce[Node] reducers collapse for monoid Int*" in {
      implicit val monoid = adpro.Monoid.intMultiplication
      reduce[Node].Laws.reducersEquivalentForMonoids[Int]
    }

    "Reduce[Node] reducers collapse for string monoid" in {
      implicit val monoid = adpro.Monoid.stringMonoid
      reduce[Node].Laws.reducersEquivalentForMonoids[String]
    }

    "Reduce[Node] reducers collapse for list monoid" in {
      implicit val monoid = adpro.Monoid.listMonoid[Boolean]
      reduce[Node].Laws.reducersEquivalentForMonoids[List[Boolean]]
    }
  }


}

