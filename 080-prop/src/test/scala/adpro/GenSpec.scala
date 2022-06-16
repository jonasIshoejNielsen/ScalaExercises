// Wasowski, Fu, Advanced Programming, IT University of Copenhagen
// Change package to adpro.SOLUTIONS to test teacher's solutions
package adpro

import java.util.concurrent._

import fpinscala.laziness.Stream
import fpinscala.laziness.Stream._
import fpinscala.state._
import fpinscala.state.RNG._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Gen => SCGen}

object Util {

  // cheating slightly as we are using another generator to seed ours
  def genRNG (implicit arbLong: Arbitrary[Long]): SCGen[RNG] =
    for { n <- arbLong.arbitrary } yield Simple (n)

  def genRNG2 (implicit arbLong: Arbitrary[Long]): SCGen[(RNG,RNG)] =
    for {
      rng1 <- genRNG
      rng2 <- genRNG suchThat { _ != rng1 }
    } yield (rng1 -> rng2)

  implicit val arbRNG: Arbitrary[RNG] = Arbitrary(genRNG)

}

class GenSpec
    extends org.scalatest.freespec.AnyFreeSpec
    with org.scalatest.matchers.should.Matchers
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {


  import Util._

  "Exercise 13 (listOfN with fixed size)" - {

    "listOfN should compile (only checks type)" in {
      """ implicit val genDouble = Gen.double
          val r4: List[List[Double]] =
                  (Gen.listOfN[Double] (3).toStream (42).take (5)).toList
      """ should compile
    }

    "listOf should compile (only checks type)" in {
      """ implicit val genDouble = Gen.double
          implicit val genInt = Gen.choose (0, 1000)
          val r5: List[List[Double]] =
                  (Gen.listOf[Double].toStream (42).take (5)).toList
      """ should compile
    }

  }

  "Exercise 12 (Prop, no tests so far; make sure it compiles" - { }

  "Exercise 11 (union)" - {
    "union is idempotent (a simple scenario)" in {
      val g = Gen.unit(42)
      for {
        n <- (g union g).listOfN (5) }
      yield { n shouldBe List (42,42,42,42,42) }
    }
  }

  "Exercise 10 (listOf, generalized size)"  - {
    "should give a list of the right size and contents" in {
      val h = Gen.unit (271)
      for { l <- Gen.unit[Double](42.311).listOf (h) }
      yield {
        all (l) shouldBe 42.311
        l should have size 271
      }
    }
  }

  "Exercise 9 (flatMap)" - {

    "A simple fixed flatMap scenario" in {
      val g = Gen.unit (42314)
      val h = Gen.unit (2)
      for { l <- h flatMap (g listOfN _) }
      yield { l shouldBe List (42314, 42314) }
    }

  }

  "Exercise 8 (listOfN with fixed size)" - {

    "should compile" in {
      """val r3: List[List[Boolean]] =
                  (Gen.boolean.listOfN(3) toStream 42 take 5).toList
      """ should compile
    }

    "should give a list of the right size and contents" in {
      for { l <- Gen.unit[Double](42.314).listOfN (42) }
      yield {
        all (l) shouldBe 42.314
        l should have size 42
      }
    }

  }

  "Exercise 7 (unit, boolean, and double)" - {
    "simple test cases" in {
      val r1 = Gen.unit(3.14).toStream(42).take(5).toList
      all (r1) shouldBe 3.14

      // These two are not really tests for the time being but let's check
      // if they type check
      "val r2: List[Boolean] = Gen.boolean.toStream(42).take(5).toList" should compile
      "val r3: List[Double] = Gen.double.toStream(42).take(5).toList" should compile
    }
  }

  "Exercise 6 (choose)" - {

    "Generated numbers must be in range [|n|;|n|+|m|+1)" in {
      forAll (SCGen.choose (0,10000) -> "start",
              SCGen.choose (0,10000) -> "m") {
        (start: Int, m: Int) =>
          val stopExclusive = start + m + 1
          val g = Gen.choose (start, stopExclusive)
          forAll ("rng") { (rng: RNG) =>
            all (g.toStream (rng).take (100).toList) should be >= start
            all (g.toStream (rng).take (100).toList) should be < stopExclusive
          }
      }
    }
  }

  "Exercise 5 (impureRandomDoubles)" - {
    "impureRandomDoubles should not be referentially transparent" in {
      import WarmupExercises.impureRandomDoubles
      (impureRandomDoubles) shouldNot be (impureRandomDoubles)
    }

    "impureDoubles1 should be different than impureDoubles2" in {
      import WarmupExercises._
      (impureDoubles1) shouldNot be (impureDoubles2)
    }
  }

  "Exercise 4 (state2stream)" - {

    "check the list of 1000 doubles" in {
        import WarmupExercises.rng1
        val oracle = Gen.state2stream (State (RNG.double)) (rng1).take (1000).toList
        WarmupExercises.someRandomDoubles should be (oracle)
    }

    "check the first 1000 doubles for the generator" in {
      forAll { rng: RNG =>
        val dbls = WarmupExercises.randomDoubles (rng).take (1000).toList
        dbls should be (Gen.state2stream (State (RNG.double)) (rng).take (1000).toList)
      }
    }

    "someRandomDoubles should be different than moreRandomDoubles" in {
      import WarmupExercises._
      (someRandomDoubles) shouldNot be (moreRandomDoubles)
    }

  }

  "Exercise 3 (State)" - {

    "s_random_int has the right type" in {
      "WarmupExercises.s_random_int: State[RNG,Int]" should compile
    }

    "s_random_int returns the right numbers" in {
      forAll { rng: RNG =>
        WarmupExercises.s_random_int.run (rng)._1 should be (rng.nextInt._1)
      }
    }

    "s_nonNegativeInt has the right type" in {
      "WarmupExercises.s_nonNegativeInt: State[RNG,Int]" should compile
    }

    "s_nonNegativeInt returns the right numbers" in {
      forAll { rng: RNG =>
        WarmupExercises.s_nonNegativeInt.run (rng)._1 should be (RNG.nonNegativeInt (rng)._1)
      }
    }

    "s_double has the right type" in {
      "WarmupExercises.s_double: State[RNG,Double]" should compile
    }

    "s_double returns the right numbers" in {
      forAll { rng: RNG =>
        WarmupExercises.s_double.run (rng)._1 should be (RNG.double (rng)._1)
      }
    }

    "Check if the numbers generated are alright" - {

      "s_random_int" in {
        WarmupExercises.random_int should be (WarmupExercises.rng1.nextInt._1)
      }
      "s_nonNegativeInt" in {
        WarmupExercises.nonNegativeInt should be (RNG.nonNegativeInt (RNG.Simple(42))._1)
      }
      "s_double" in {
        WarmupExercises.double should be (RNG.double (RNG.Simple(42))._1)
      }
    }

  }

  "Exercise 2 (nextInt)" - {

    "The first integer (x)" in {
      WarmupExercises.x should be (WarmupExercises.rng1.nextInt._1)
    }

    "The second integer (y)" in {
      WarmupExercises.y should be (WarmupExercises.rng1.nextInt._2.nextInt._1)
    }
  }

  "Exercise 1 (RNG)" - {
    "One test :) [scenario]" in {
      WarmupExercises.rng1 should equal (RNG.Simple (42))
    }
  }


}
