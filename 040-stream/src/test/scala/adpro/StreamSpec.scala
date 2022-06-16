// wasowski, Advanced Programming, IT University of Copenhagen
// change package to adpro.SOLUTIONS to test teacher's solutions
package adpro

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

class StreamSpec
    extends org.scalatest.freespec.AnyFreeSpec
    with org.scalatest.matchers.should.Matchers
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  import Stream._

  "Exercise 13" - {
    "naturals.map_(_+1).take(10).toList should give correct result" in {
      naturals.map_(_+1).take(10).toList shouldBe naturals.map(_+1).take(10).toList
    }
    "naturals.take_(10).toList should give correct result" in {
      naturals.take_(10).toList shouldBe naturals.take(10).toList
    }
    "naturals.takeWhile_ {_ < 1000000000 }.drop (100).take (50).toList should give correct result" in {
      naturals.takeWhile_ { _ < 1000000000 }.drop (100).take (50).toList.size shouldBe naturals.takeWhile2 { _ < 1000000000 }.drop (100).take (50).toList.size
    }
    "naturals.zipWith_[Int,Int](_+_)(naturals).take(10).toList should give correct result" in {
      naturals.zipWith_[Int,Int](_+_)(naturals).take(10).toList shouldBe naturals.map (_*2).take(10).toList
    }
  }
  "Exercise 12" - {
    "fibs1.take(10).toList should give correct result" in {
      fibs1.take(10).toList shouldBe fibs.take (10).toList
    }
    "from1(100).take(10).toList should give correct result" in {
      from1(100).take(10).toList shouldBe from(100).take(10).toList
    }
  }
  "Exercise 11" - {
    "Unfold with immediate failure should give empty" in {
      Stream.unfold (()) { case () => None } should be (Empty)
    }
  }
  "Exercise 10" - {   //added by me
    "fibs.take (10).toList should give first 10" in {
      fibs.take (10).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    }
  }
  "Exercise 8" - {
    "naturals.map (_*2).drop (30).take (50).toList gives correct list" in {
      naturals.map (_*2).drop (30).take (50).toList shouldBe List.range(31, 81).map(_*2)
    }
    "naturals.drop (42).filter (_%2 ==0).take (30).toList gives correct list" in {
      naturals.drop (42).filter (_%2 ==0).take (30).toList shouldBe List.range(1, 200).drop (42).filter (_%2 ==0).take (30)
    }
    "naturals.take(10).append(naturals).take(20).toList gives correct list" in {
      naturals.take(10).append(naturals).take(20).toList shouldBe (List.range(1, 200).take(10) ++ List.range(1, 200)).take(20)
    }
    "naturals.flatMap (to _).take (100).toList gives correct list" in {
      naturals.flatMap (to _).take (100).toList shouldBe List.range(-1, 200).map(-1*_).take(100)
    }
    "naturals.flatMap (x =>from (x)).take (100).toList gives correct list" in {
      naturals.flatMap (x =>from (x)).take (100).toList shouldBe List.range(1, 200).take(100)
    }
  }

  "Exercise 7" - {
	  "headOption2 (empty)" in {
      Stream.empty.headOption2 should be (None)
	  }
  }

  "Exercise 6" - {
	  "takeWhile2 (_ < 10) gives 9 naturals" in {
      naturals.takeWhile2 { _ < 10 }.toList.size shouldBe (9)
	  }
  }

  "Exercise 5" - {

    "The succesful test from the exercise text" in {
      naturals.forAll (_ < 0) should be (false)
    }
  }


  "Exercise 4" - {
	  "takeWhile (_ < 10) gives 9 naturals" in {
      naturals.takeWhile { _ < 10 }.toList.size shouldBe (9)
	  }

    "naturals.takeWhile {_ < 1000000000 }.drop (100).take (50).toList should not crash" in {
      naturals.takeWhile { _ < 1000000000 }.drop (100).take (50).toList.size should be (50)
    }
  }

  "Exercise 3" - {

	  "naturals.take (3) is Stream (1,2,3) " in {
	  	  val s :Stream[Int]= naturals.take(3)
        s.toList should be (List (1,2,3))
    }

    "naturals.drop (3) is Stream (4,5,6,...) " in {
	  	  val s :Stream[Int]= naturals.drop(3)
        s.take (3).toList should  be (List (4,5,6))
    }

    "naturals.take (1000000000).drop (41).take (10).toList should not crash" in {
      val l = naturals.take(1000000000).drop(41).take(10).toList
      l.size should be (10)
    }
  }

  "Exercise 2" - {

	  "The Stream(1,2,3).toList is List(1,2,3)" in {
        // 'empty' by itself crashes with the internal DSL of the testing
        // framework (there is a matcher called 'empty')
	  	  val s: Stream[Int] = cons(1, cons(2, cons (3, Stream.empty)))
        s.toList should be (List (1,2,3))
	  }
  }

  "Exercise 1" - {

    "The first element of Stream.from (3) is 3" in {
      from (3).headOption should be (Some (3))
    }

    "The second element of Stream.from (3) is 4" in {
      from (3).tail.headOption should be (Some (4))
    }

	  "The first element of Stream.to (3) is 3" in {
      to (3).headOption should be (Some (3))
    }

    "The second element of Stream.to(3) is 2" in {
      to(3).tail.headOption should be (Some (2))
    }

	  "The first element of naturals is 1" in {
	  	naturals.headOption should be (Some (1))
	  }

  }

}
