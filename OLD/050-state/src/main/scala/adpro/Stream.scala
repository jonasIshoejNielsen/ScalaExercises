// Advanced Programming
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

package adpro

sealed trait Stream[+A] {

  import Stream._

  def headOption: Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def tail: Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f: (A, =>B) => B): B = this match {
      case Empty => z
      case Cons (h,t) => f (h(), t().foldRight (z) (f))
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note. foldLeft is eager; cannot be used to work with infinite streams. So
  // foldRight is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
      case Empty => z
      case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

  def exists (p : A => Boolean) :Boolean = this match {
      case Empty => false
      case Cons (h,t) => p(h()) || t().exists (p)
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }


  // Exercise 2

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h() :: t().toList
  }

  // Exercise 3

  def take (n: Int): Stream[A] = this match {
      case Empty => 
        Empty
      case Cons (h, t) => 
        if (n == 0) Empty 
        else Cons (h, () => t ().take (n-1))
  }

  @annotation.tailrec
  final def drop (n: Int): Stream[A] = this match {
      case Empty => 
        Empty
      case Cons (_,t) => 
        if (n == 0) this 
        else t ().drop (n-1)
  }

  // Exercise 4

  def takeWhile (p: A => Boolean): Stream[A] = ???

  //Exercise 5
  
  def forAll (p: A => Boolean): Boolean = ???

  //Exercise 6
  
  def takeWhile2 (p: A => Boolean): Stream[A] = ???

  //Exercise 7

  def headOption2: Option[A] = ???

  //Exercise 8 The types of these functions are omitted as they are a part of the exercises

  def map = ???

  def filter = ???

  def append = ???

  def flatMap = ???

  //Exercise 09
  //Put your answer here:

  //Exercise 10
  //Put your answer here:

  // Exercise 13

  def map_ = ???
  def take_ = ???
  def takeWhile_ = ???
  def zipWith_ = ???

}


case object Empty extends Stream[Nothing]
case class Cons[+A] (h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons (as.head, apply (as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq


  // Exercise 1

  def from (n: Int): Stream[Int] = ???

  def to (n: Int): Stream[Int] = ???

  lazy val naturals: Stream[Int] = ???

  //Exercise 11

  def unfold [A, S] (z: S) (f: S => Option[(A, S)]): Stream[A] =
    f(z).map ((v: (A,S)) => cons(v._1, unfold (v._2) (f))).getOrElse(Empty)


  // Exercise 12

  def fib2  = ???
  def from2 = ???

}

