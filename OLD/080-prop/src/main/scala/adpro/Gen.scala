// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: Join F# in august
//
// AUTHOR1: Jonas Ishøj Nielsen
// TIME1: 2 hours, 00 min <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: August Wester
// TIME2: 2 hours, 00 min <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR3: Anders Degn Lapiki
// TIME3: 2 hours, 00 min <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file shall always compile and run after you are done with each exercise
// (if you do them in order).  Please compile and test frequently. Of course,
// some tests will be failing until you finish. Only hand in a solution that
// compiles and where tests pass for all parts that you finished.    The tests
// will fail for unfnished parts.  Comment such out.
//
// Before starting to work on the exercises, familiarize yourself with the the
// content already in the file (it has been explained in chapter 8, but it is
// useful to see it all together in one file).

package adpro

import fpinscala.laziness.Stream._
import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.state.RNG._

object WarmupExercises {


  // Exercise 1
  lazy val rng1: RNG = RNG.Simple(42)


  // Exercise 2
  lazy val x: Int = rng1.nextInt._1
  lazy val y: Int = rng1.nextInt._2.nextInt._1

  // Exercise 3
  lazy val s_random_int: State[RNG,Int] = State(_.nextInt)

  lazy val s_nonNegativeInt: State[RNG,Int] = s_random_int.map { a =>
    if (a < 0) -(a + 1) else a
  }

  lazy val s_double: State[RNG,Double] = s_nonNegativeInt.map { a =>
    a / (Int.MaxValue.toDouble + 1)
  }

  lazy val random_int: Int = s_random_int.run(rng1)._1
  lazy val nonNegativeInt: Int =  s_nonNegativeInt.run(rng1)._1
  lazy val double: Double = s_double.run(rng1)._1

  import Gen.state2stream

  // Exercise 4
  def randomDoubles (seed: RNG): Stream[Double] =
    Gen.state2stream(s_double)(seed)

  lazy val someRandomDoubles: List[Double] =
    randomDoubles(rng1).take(1000).toList

  lazy val moreRandomDoubles: List[Double] =
    randomDoubles(RNG.Simple(69)).take(1000).toList

  // Exercise 5
  def impureRandomDoubles: Stream[Double] = {
    val seed = System.currentTimeMillis.toInt
    randomDoubles(RNG.Simple(seed))
  }

  lazy val impureDoubles1: Stream[Double] = impureRandomDoubles

  lazy val impureDoubles2: Stream[Double] = impureRandomDoubles
}

// A generator will use a random number generator RNG in its state, to create
// random instances (but perhaps also some other staff)
case class Gen[A] (sample: State[RNG,A]) {

  // Let's convert generator to streams of generators
  def toStream (seed: Long): Stream[A] =
    Gen.state2stream (this.sample) (RNG.Simple (seed))

  def toStream (rng: RNG): Stream[A] =
    Gen.state2stream (this.sample) (rng)

  // Exercise 8

  def listOfN (n: Int): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(this.sample)))

  // Exercise 9

  def flatMap[B] (f: A => Gen[B]): Gen[B] = 
    Gen(this.sample.flatMap(f andThen (_.sample)))

  // It would be convenient to also have map  (uses flatMap)

  def map[B] (f: A => B): Gen[B] = this.flatMap (a => Gen.unit[B] (f(a)))

  // Exercise 10

  def listOfN (size: Gen[Int]): Gen[List[A]] = 
    size.flatMap(s => Gen(State.sequence(List.fill(s)(this.sample))))
    
  // Exercise 11

  def union (that: Gen[A]): Gen[A] = 
    Gen.boolean.flatMap(b => if (b) this else that)



  // Exercise 12 continues in the companion object (below)
}

object Gen {

  // A convenience function to convert states (automata) to streams (traces)
  // It would be better to have it in State, but I am not controlling
  // State.scala.

  private[adpro]  def state2stream[A] (s :State[RNG,A]) (seed :RNG) :Stream[A] =
    s.run(seed) match { case (n,s1) => cons (n, state2stream (s) (s1)) }

  // A generator for Integer instances

  def anyInteger: Gen[Int] = Gen(State(_.nextInt))

  // Exercise 6

  def choose (start: Int, stopExclusive: Int): Gen[Int] = 
    anyInteger.map(i=>(Math.abs(i)%(stopExclusive-start))+start)

  // Exercise 7

  def unit[A] (a: =>A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = anyInteger.map(_ % 2 == 0)

  def double: Gen[Double] = anyInteger.map(_ / (Int.MaxValue.toDouble + 1))

  // (Exercise 8 is found in the Gen class above)

}

// This is the Prop type implemented in [Chiusano, Bjarnasson 2015]

object Prop {

  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  // the type of results returned by property testing

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified (
    failure: FailedCase,
    successes: SuccessCount
  ) extends Result {
      def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  def forAll[A] (as: Gen[A]) (f: A => Boolean): Prop = Prop {

    (n,rng) => as.toStream (rng).zip (Stream.from(0)).take(n).map {

      case (a,i) => try {
        if (f (a)) Passed else Falsified (a.toString, i)
      } catch { case e: Exception => Falsified (buildMsg(a, e), i) }

    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A] (s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

import Prop._

case class Prop (run: (TestCases,RNG)=>Result) {

  // (Exercise 12)

  def && (that: Prop): Prop = Prop((tc,rng) =>{
      var r1 = this.run(tc,rng) 
      if (r1.isFalsified) r1
      else
        that.run(tc,rng)
    }
  )


  def || (that: Prop): Prop = Prop((tc,rng) =>{
      var r1 = this.run(tc,rng) 
      if (!r1.isFalsified) r1
      else
        that.run(tc,rng)
    }
  )
}