// Andrzej Wąsowski, Advanced Programming
// based on fpinscala exercises
// package adpro.SOLUTIONS to test solutions
package adpro

import scala.language.implicitConversions

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalactic.Equality

class ImportStudentMonoidSpec extends MonoidExercisesSpec

class MonoidSpec
    extends org.scalatest.freespec.AnyFreeSpec
    with org.scalatest.matchers.should.Matchers
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  val M = Monoid

  "Exercise 5 (foldMap)" - {

    implicit val intMonoid = M.intAddition

    "foldMap a list with identity" in
      forAll { (l: List[Int], n: Int) =>
        M.foldMap (l map {_ => n}) (identity[Int]) should
          be (l.size * n) }

    "foldMap with incr/decr" in
      forAll { (l: List[Int], n: Int) =>
        M.foldMap (l map {_ => n}) (_ + 1) should
          be (l.size * (n+1)) }

  }

  "Exercise 3 (endMonoid)" - {

    "endoMonoid is a monoid" in {

      // Like in most langauges, functions cannot be compared for equality in
      // Scala.  The equality test between two function values gives false
      // (unless it is really the same lambda thunk we are comparing).  We
      // 'cheat' by replacing a hard equality test by a property test for
      // equality based on random sampling.
      //
      // Override equality used for functions to use in monoid laws below
      // (picked up by implicit parameters)

      implicit def intFnEq: Equality[Int => Int] =
        new Equality[Int => Int] {
          def areEqual (f: Int => Int, g: Any): Boolean = {
            // This is nasty but I am tired of fighting type erasures on JVM
            // and we know that this will only be used on functions Int => Int
            val g1 = g.asInstanceOf[Int => Int]
            forAll { x: Int => f (x) should be (g1 (x)) }
            true
          }
        }

      val m = M.endoMonoid[Int]
      M.endoMonoid[Int].Laws.monoid // this will use intFnEq
    }

  }

  "Monoid tests for strings and lists" - {

    "stringMonoid is a monoid" in
      M.stringMonoid.Laws.monoid

    "listMonoid[Char] is a monoid" in
      M.listMonoid[Char].Laws.monoid
  }



}
