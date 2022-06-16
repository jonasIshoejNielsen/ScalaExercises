// Andrzej Wąsowski, Advanced Programming
// based on fpinscala exercises
// package adpro.SOLUTIONS to test solutions
package adpro

import scala.language.implicitConversions

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

class FoldableSpec
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers
  with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  def F[A] = Foldable.foldableList[A]

  "Exercise 12 (toList)" - {

    "toList on list foldable is identity" in
      forAll { l: List[Int] =>
        F.toList (l) should
          be (l) }
  }

  "Exercise 11 (FoldableList)" - {

    "sum a list using a Foldable instance and foldLeft" in
      forAll { (l: List[Int], k: Int) =>
        F.foldLeft (l) (k) (_ + _) should
          be (l.sum + k) }

    "sum a list using a Foldable instance and foldRight" in
      forAll { (l: List[Int], k: Int) =>
        F.foldRight (l) (-k) (_ + _) should
          be (l.sum - k) }

    "size a list using a Foldable instance with foldMap" in
      forAll { l: List[Unit] =>
        F.foldMap (l) (_ => 1) (Monoid.intAddition) should
          be (l.size) }
  }

}
