// Andrzej Wąsowski, Advanced Programming
// based on fpinscala exercises
// package adpro.SOLUTIONS to test solutions
package adpro

import scala.language.implicitConversions
import scala.language.higherKinds

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

class ImportStudentMonadSpec extends MonadExercisesSpec

class  MonadSpec
    extends org.scalatest.freespec.AnyFreeSpec
    with org.scalatest.matchers.should.Matchers
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  val M = Monad

  // Identity Monad, the simplest monad, good for testing

  type Id[A] = A
  val idMonad = new Monad[Id] {

    def unit[A] (a: => A): Id[A] = a

    def flatMap[A,B] (a: Id[A]) (f: A => Id[B]): Id[B] = f(a)
  }

  "Exercise 19 (Kleisli)" - {

    "Kleisli composition in Id" in
      forAll { (f: Int => Id[Int], g: Int => Id[String]) =>
        forAll { x: Int =>
          idMonad.compose[Int,Int,String] (f,g) (x) should
            be (g (f (x))) } }

    "Kleisli composition in Option" in
      forAll { (f: Int => Int, g: Int => String) =>
        val fo: Int => Option[Int] = x => Some (f (x))
        val go: Int => Option[String] = x => Some (g (x))

        forAll { x: Int =>
          M.optionMonad.compose[Int,Int,String] (fo,go) (x) should
            be (Some (g (f (x)))) } }
  }


  "Exercise 18 (replicateM)" - {

    "replicateM in Id" in
      forAll (Gen.choose (0, 100) -> "n") { n: Int =>
        idMonad.replicateM (n, n) should
         be (List.fill (n) (n))
      }

    "replicateM in Option" in
      forAll (Gen.choose (0, 100) -> "n") { n: Int =>
        M.optionMonad.replicateM (n+1, None) should
         be (None)
      }


  }

  "Exercise 17 (sequence)" - {

    "sequence in idMonad (2)" in
      forAll { l: List[Unit] =>
        idMonad.sequence[Unit] (l) should
         be (l)
      }

    "sequence in idMonad (1)" in
      forAll { l: List[Int] =>
        idMonad.sequence[Int] (l) should
         be (l)
      }

    "regression in optionMonad map2 with None None" in
      forAll { n: Int =>
        def f (a: Unit, b:Unit ): Unit = ()
        M.optionMonad.map2[Unit,Unit,Unit] (None, None) (f) should
         be (None) }

    "sequence in optionMonad (2)" in
    forAll { n: Int =>
        val lfa = List.fill (Math.abs (n % 100) + 1) (None)
        M.optionMonad.sequence (lfa) should
         be (None)
      }

    "sequence in optionMonad (1)" in
      forAll { l: List[Int] =>
        val lfa = l.map (Some (_))
        M.optionMonad.sequence (lfa) should
         be (Some (l))
      }

  }

}
