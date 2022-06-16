// wasowski, Advanced Programming, IT University of Copenhagen
package fpinscala.laziness
import scala.language.higherKinds

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

// If you  comment out all the  import lines below, then  you test the
// Scala  Standard Library  implementation of  Streams. Interestingly,
// the standard library streams are stricter than those from the book,
// so some laziness tests fail on them.

import stream00._    // uncomment to test the book solution
//import stream01._ // uncomment to test the broken headOption implementation
//import stream02._ // uncomment to test another version that breaks headOption

class StreamSpecAuweAlapJoin
    extends org.scalatest.freespec.AnyFreeSpec 
    with org.scalatest.matchers.should.Matchers
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  import Stream._

  // A simple converter of lists to streams
  def list2stream[A] (la :List[A]): Stream[A] =
    la.foldRight (Stream.empty[A]) (cons[A](_,_))

  // There  is  a name  clash  between  Stream.empty and  the  testing
  // library, so we need to qualify Stream.empty

  // An example generator  of random finite non-empty  streams (we use
  // the  built in  generator of  lists and  convert them  to streams,
  // using the above converter)
  //
  // 'suchThat'  filters  out  the  generated instances  that  do  not
  // satisfy the predicate given in the right argument.
  
  type UInt = Int
  
  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] =
    for {
      la <- arbitrary[List[A]] suchThat { _.nonEmpty }
    } yield list2stream (la)

  def genNonNegativeInteger (implicit arbA :Arbitrary[Int]) :Gen[UInt] =
    Gen.choose(0, Int.MaxValue)

  def infinite[A] (a :A) :Stream[A] =
      cons(a, infinite(a))

  implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
  implicit def arbUInt      = Arbitrary[UInt] (genNonNegativeInteger)

  "headOption" - {

    // a scenario test:

    "returns None on an empty Stream (01)" in {
      (Stream.empty.headOption) shouldBe (None)
    }


    // two property tests:

    "returns the head of a singleton stream packaged in Some (02)" in {
      forAll { (n :Int) => cons (n, Stream.empty).headOption should be (Some (n)) }
    }

    "returns the head of random stream packaged in Some (02)" in {
      // The implict makes the generator available in the context

      // This property uses our generator of non empty streams thanks to the
      // above implicit declaration
      forAll { (s :Stream[Int]) => s.headOption shouldNot be (None) }
    }
    
    "headOption should not force the tail of the stream" in {
      forAll { (n :Int) => cons (n, cons(throw new Exception("headOption forced tail"), Stream.empty)).headOption should be (Some (n)) }
    }

  }

  "take" - {
    "take should not force any heads nor any tails of the Stream it manipulates" in {
      forAll { (s :Stream[Int], n: UInt) => s.map(v => throw new Exception("take forced elements")).take(n) }
    }

    "take (n) does not force (n+1)st head ever (even if we force all elements of take(n))" in {
      forAll { (s :Stream[Int], n: UInt) =>
        val temp = s.take(n)
        val size = temp.toList.size
        temp.append(cons(throw new Exception("take forced elements"), Stream.empty )).take(Math.min(n,size)).toList
      }
    }
    
    "s.take (n).take (n) == s.take (n) for any Stream s and any n (idempotency)" in {
      forAll { (s :Stream[Int], n: UInt) => s.take(n).take(n).toList should be (s.take(n).toList) }
    }
    
    "take (n) returns at max n elements" in {
      forAll { (s :Stream[Int], n: UInt) =>
        val temp = s.take(n)
        Math.max(temp.toList.size,n) should be (n)
      }
    }

    "s.take(n) when n > size returns min(size, n)" in{
      forAll { (s :Stream[Int], n: UInt) => s.take(n).toList.size should be (Math.min(s.toList.size, n)) }
    }
  }

  "drop" - {
    "s.drop (n).drop (m) == s.drop (n + m) for any n, m (additivity)" in{
      forAll { (s :Stream[Int], n: UInt, m: UInt) => 
        if (Int.MaxValue - n > m)
          s.drop(n).drop(m).toList should be (s.drop(n + m).toList) }
    }
    

    "s.drop (n) does not force any of the dropped elements heads" in{
      forAll { (s :Stream[Int], n: UInt) => s.map(v => throw new Exception("drop forced elements")).drop(n) }
    }

    "s.take(n).drop (n) should be empty" in{
      forAll { (s :Stream[Int], n: UInt) => s.take(n).drop(n) should be (Stream.empty) }
    }

    "The above should hold even if we force some stuff in the tail" in {
      val s1 = cons(throw new Exception("forced dropped elements"), cons(throw new Exception("forced dropped elements"), cons(1, Stream.empty)))
      s1.drop(2).toList
    }
    "s.drop(n) when n > size returns empty" in{
      forAll { (s :Stream[Int], n: UInt) => 
        if (s.take(n).toList.size < n)
          s.drop(n) should be (Stream.empty) }
    }
    "s.drop (n) actually drops" in{
      forAll { (s :Stream[Int], n: UInt) => 
      val temp = s.take(n).take(n)    //avoid overflow
      temp.drop(n).toList should be (temp.toList.drop(n)) }
    }
  }

  "map" - {

    "x.map (id) == x (where id is the identity function)" in{
      forAll { (s :Stream[Int]) => s.map(identity).toList should be (s.toList)}
    }
    "'map' terminates on infinite streams" in {
      forAll { (s :Int) => infinite(s).map(identity)}
    }
    
    "x.map actually maps" in{
      forAll { (s :Stream[Int], f: Int=>Int, n: UInt) => 
        s.map(f).take(n).toList should be (s.take(n).toList.map(f))
      }
    }
  }
  "append" - {
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
