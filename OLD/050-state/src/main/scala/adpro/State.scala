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
// This file is compiled with 'sbt compile' and tested with 'sbt test'.

package adpro

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG (seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {
      
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL 
      
      // The next state, which is an `RNG` instance created from the new seed.
      val nextRNG = SimpleRNG (newSeed) 
      
      // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt 
      
      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG) 

    }
    
  }

  // Exercise 1 (CB 6.1)

  def nonNegativeInt (rng: RNG): (Int, RNG) = rng.nextInt match{
    case (Int.MinValue, rng) => nonNegativeInt(rng)
    case (i, rng) => (math.abs(i), rng)
  }

  // Exercise 2 (CB 6.2)

  def double (rng: RNG): (Double, RNG) = nonNegativeInt(rng) match{
    case (i, rng) => (i.toDouble / Int.MaxValue, rng)
  }
  //is uniform between the possible values, since nonNegativeInt is uniform

  // Exercise 3 (CB 6.3)

  def intDouble (rng1: RNG): ((Int, Double), RNG) = nonNegativeInt(rng1) match{
    case (i, rng2) => double(rng2) match {
      case (d, rng3) => ((i,d), rng3)
    }
  }

  def doubleInt (rng: RNG) = intDouble(rng) match{
    case ((i,d), rng) => ((d,i), rng)
  }

  def boolean (rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i % 2 == 0, rng2) }

  // Exercise 4 (CB 6.4)

  def ints (count: Int) (rng: RNG): (List[Int], RNG) =
    if (count <= 0) (Nil, rng) else rng.nextInt match {
      case (i, rng) => (ints (count-1) (rng)) match {
        case (lst, rng) => (i :: lst, rng)
      }
    }

  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A] (a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B] (s: Rand[A]) (f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s (rng)
      (f (a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map (nonNegativeInt) (i => i - i % 2)

  // Exercise 5 (CB 6.5) (Lazy is added so that the class does not fail
  // at load-time without your implementation).

  lazy val _double: Rand[Double] = map (nonNegativeInt) (i => i.toDouble / Int.MaxValue)

  // Exercise 6 (CB 6.6)

  def map2[A,B,C] (ra: Rand[A], rb: Rand[B]) (f: (A, B) => C): Rand[C] = 
    rng => {
      val (a, rng2) = ra (rng)
      val (b, rng3) = rb (rng2)
      (f (a,b), rng3)
    }

  // this is given in the book

  def both[A,B] (ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2 (ra, rb) ((_, _))

  lazy val randIntDouble: Rand[(Int, Double)] = both (int, double)

  lazy val randDoubleInt: Rand[(Double, Int)] = both (double, int)

  // Exercise 7 (6.7)

  def sequence[A] (fs: List[Rand[A]]): Rand[List[A]] = rng =>
    fs.foldRight[(List[A], RNG)] ((Nil, rng)) ((a,acc) => a(acc._2) match {
      case (v, rng) => (v :: acc._1, rng) 
    })
    

  def _ints (count: Int): Rand[List[Int]] = sequence(List.fill(count) (int))

  // Exercise 8 (6.8)

  def flatMap[A,B] (f: Rand[A]) (g: A => Rand[B]): Rand[B] = 
    rng => {
      val (a, rng2) = f (rng)
      val (b, rng3) = g (a) (rng2)
      (b, rng3)
    }

  def nonNegativeLessThan (n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => rng => (i%n,rng))

}

import State._     //type State[S,+A] = S => (A,S)

case class State[S, +A](run: S => (A, S)) {

  // Exercise 9 (6.10)

  def map[B] (f: A => B): State[S, B] = 
    State(s => {
      val (a, s2) = this.run (s)
      (f (a), s2)
    })

  def map2[B,C] (sb: State[S, B]) (f: (A, B) => C): State[S, C] = 
    State(s => {
      val (a, s2) = this.run (s)
      val (b, s3) = sb.run (s2)
      (f (a,b), s3)
    })


  def flatMap[B] (f: A => State[S, B]): State[S, B] = 
    State(s => {
      val (a, s2) = this.run(s)
      val (b, s3) = f (a).run (s2)
      (b, s3)
    })

}

object State {

  import adpro.Stream

  type Rand[A] = State[RNG, A]

  def unit[S, A] (a: A): State[S, A] =
    State (s => (a, s))

  // Exercise 9 (6.10) continued

  def sequence[S,A] (sas: List[State[S, A]]): State[S, List[A]] = State(s =>
    sas.foldRight[(List[A], S)] ((Nil, s)) ((a,acc) => a.run(acc._2) match {
      case (v, s) => (v::acc._1, s)
    }))

  // This is given in the book:

  def modify[S] (f: S => S): State[S, Unit] = for {
    // Get the current state and assigns it to `s`.
     s <- get 
     // Set the new state to `f` applied to `s`.
     _ <- set (f (s)) 
  } yield ()

  def get[S]: State[S, S] = State (s => (s, s))

  def set[S] (s: S): State[S, Unit] = State (_ => ((), s))

  def random_int: Rand[Int] =  State (_.nextInt)

  // Exercise 10

  def state2stream[S,A] (s: State[S,A]) (seed: S): Stream[A] =    
    Stream.unfold[A,S] (seed) { seed => Some(s.run(seed))}

  // Exercise 11 (lazy is added so that the class does not crash at load time
  // before you provide an implementation).

  lazy val random_integers : Stream[Int] = state2stream(random_int)(RNG.SimpleRNG(42))

  //val took10: List[Int] = random_integers.take(10).toList
  
}