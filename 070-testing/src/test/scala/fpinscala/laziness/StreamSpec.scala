// wasowski, Advanced Programming, IT University of Copenhagen
package fpinscala.laziness
import scala.language.higherKinds

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import stream00._    // uncomment to test the book solution (should pass your tests)
// import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption

class StreamSpec
    extends org.scalatest.freespec.AnyFreeSpec
    with org.scalatest.matchers.should.Matchers
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  import Stream._

  // A simple converter of lists to streams

  def list2stream[A] (la: List[A]): Stream[A] =
    la.foldRight (Stream.empty[A]) (cons[A](_,_))

  // There  is  a name  clash  between  Stream.empty and  the  testing
  // library, so we need to qualify Stream.empty

  // An example generator  of random finite non-empty  streams (we use
  // the  built in  generator of  lists and  convert them  to streams,
  // using the above converter)
  //
  // 'suchThat'  filters  out  the  generated instances  that  do  not
  // satisfy the predicate given in the right argument.

  def genNonEmptyStream[A] (implicit arbA: Arbitrary[A]): Gen[Stream[A]] =
    for {
      la <- arbitrary[List[A]] suchThat { _.nonEmpty }
    } yield list2stream (la)

  //MY HELPER FUNCTION:
  implicit val arbIntStream =
      Arbitrary[Stream[Int]] (genNonEmptyStream[Int])


  "headOption" - {

    // Exercise 1 (no coding, understand)

    // A scenario test:

    "returns None on an empty Stream (01)" in {

      Stream.empty.headOption shouldBe (None)
    }


    // Two property tests:

    "returns the head of a singleton stream packaged in Some (02)" in {

      forAll { (n: Int) =>
        cons (n, Stream.empty).headOption should be (Some (n))
      }
    }

    "returns the head of random stream packaged in Some (02)" in {

      // Make the generator available in the context
      implicit val arbIntStream =
        Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      // Uses our generator of non empty streams
      // thanks to the implicit declaration above
      forAll { (s: Stream[Int]) =>
        s.headOption shouldNot be (None)
      }
    }

    // Exercise 2 (add here)
    
    "headOption should not force the tail of the stream" in {
      forAll { (n :Int) => cons (n, cons(throw new Exception("headOption forced tail"), Stream.empty)).headOption should be (Some (n)) }
    }

  }

  "take" - {

    // Exercise 3
    "take should not force any heads nor any tails of the Stream it manipulates" in {
      //ADDED THE IMPLICIT AT THE TOP
      forAll { (s :Stream[Int], n: Int) => s.map(v => throw new Exception("take forced elements")).take(n) }
    } 

    // Exercise 4
    "take (n) does not force (n+1)st head ever (even if we force all elements of take(n))" in {
      forAll { (s :Stream[Int], n: Int) =>
        val temp = s.take(n)
        val size = temp.toList.size
        temp.append(cons(throw new Exception("take forced elements"), Stream.empty )).take(Math.min(n,size)).toList
      }
    }

    // Exercise 5
    "s.take (n).take (n) == s.take (n) for any Stream s and any n (idempotency)" in {
      forAll { (s :Stream[Int], n: Int) => s.take(n).take(n).toList should be (s.take(n).toList) }
    }

    //EXTRA NOT EXERCISE:
    "take (n) returns at max n elements" in {
      forAll { (s :Stream[Int], n: Int) =>
        val temp = s.take(n)
        if(n>=0)
          Math.max(temp.toList.size,n) should be (n)
        else
          Math.max(temp.toList.size,n) should be (0)
      }
    }

    "s.take(n) when n > size returns min(size, n)" in{
      forAll { (s :Stream[Int], n: Int) =>
        if(n>=0)
          s.take(n).toList.size should be (Math.min(s.toList.size, n))
        else
          s.take(n).toList.size should be (0)
      }
    }

  }

  "drop" - {

    // Exercise 6
    "s.drop (n).drop (m) == s.drop (n + m) for any n, m (additivity)" in{
      forAll { (s :Stream[Int], n: Int, m: Int) => 
        if (Int.MaxValue - n > m && n>=0 && m>=0)
          s.drop(n).drop(m).toList should be (s.drop(n + m).toList) }
    }

    // Exercise 7
    "s.drop (n) does not force any of the dropped elements heads" in{
      forAll { (s :Stream[Int], n: Int) => s.map(v => throw new Exception("drop forced elements")).drop(n) }
    }

    //EXTRA NOT EXERCISE:
    "s.take(n).drop (n) should be empty" in{
      forAll { (s :Stream[Int], n: Int) => s.take(n).drop(n) should be (Stream.empty) }
    }

    "The above should hold even if we force some stuff in the tail" in {
      val s1 = cons(throw new Exception("forced dropped elements"), cons(throw new Exception("forced dropped elements"), cons(1, Stream.empty)))
      s1.drop(2).toList
    }
    "s.drop(n) when n > size returns empty" in{
      forAll { (s :Stream[Int], n: Int) => 
        if (s.take(n).toList.size < n)
          s.drop(n) should be (Stream.empty) }
    }
    "s.drop (n) actually drops" in{
      forAll { (s :Stream[Int], n: Int) => 
      val temp = s.take(n).take(n)    //avoid overflow
      temp.drop(n).toList should be (temp.toList.drop(n)) }
    }
  }


  "map" - {

    // Exercise 8
    "x.map (id) == x (where id is the identity function)" in{
      forAll { (s :Stream[Int]) => s.map(identity).toList should be (s.toList)}
    }

    // Exercise 9
    "'map' terminates on infinite streams" in {
      forAll { (s :Int) => from(s).map(identity)}
      forAll { (s :Int) => constant(s).map(identity)}
      forAll { (s :Int) => ones.map(identity)}
    }
    
    //EXTRA NOT EXERCISE:
    "x.map actually maps" in{
      forAll { (s :Stream[Int], f: Int=>Int, n: Int) => 
        s.map(f).take(n).toList should be (s.take(n).toList.map(f))
      }
    }
  }

  "append" - {

    // Exercise 10
    "append actually appends" in {
      forAll { (s: Stream[Int], n: Int) =>
        s.append(cons(n, Stream.empty)).toList.last shouldBe n
      }
    }

    "append leaves original elements unmodified" in {
      forAll { (s: Stream[Int], n: Int) => {
        val size = s.toList.size
        s.append(cons(n, Stream.empty)).take(size).toList shouldBe s.toList
      }}
    }

    "append works on empty stream" in {
      forAll {(n: Int) => {
        val s = Stream.empty.append(cons(n, Stream.empty))
        s.headOption shouldBe Some(n)
      }}
    }
    
    "append stack" in {
      forAll { (s: Stream[Int], n: Int, m: Int) => {
        val size = s.toList.size
        s.append(cons(n, Stream.empty)).append(cons(m, Stream.empty)).drop(size).toList shouldBe cons(n,cons(m, Stream.empty)).toList
      }}
    }
  }

}
