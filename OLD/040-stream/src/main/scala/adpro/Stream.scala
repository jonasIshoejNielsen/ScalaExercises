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
    case Empty => Empty
    case Cons(h,t) => if (n <= 0) Empty else Cons(h, ()=>t().take(n-1))
  }

  def drop (n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) => if (n <= 1) t() else t().drop(n-1)
  }
  /*
  naturals.take (1000000000).drop (41).take (10).toList
  doesn't fail because the take only creates one cons, as the tail is a function not yet run.
  For the same reason the drop doesn't iterate over 1000000000 but only the first element
  */

  // Exercise 4

  def takeWhile (p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) => if (p(h())) Cons(h, ()=>t().takeWhile(p)) else Empty
  }
  //test is in "naturals.takeWhile {_ < 1000000000 }.drop (100).take (50).toList should not crash"
  //why: same reason as exercise 3, tail is a function not yet run


  //Exercise 5
  def forAll (p: A => Boolean): Boolean = !exists(x => !p(x))
  /*
    naturals.forAll (_ >=0)
    fails because it has to evaluate each element, and at no time can stop early

  */


  //Exercise 6
  def takeWhile2 (p: A => Boolean): Stream[A] = 
    foldRight[Stream[A]] (Empty) { (a, acc) =>
      if (p(a)) cons(a, acc) else Empty
    }
  //see StreamSpec.scala for the test

  //Exercise 7
  def headOption2: Option[A] = 
    foldRight[Option[A]] (None) { (a, _) =>
      Some(a)
    }


  //Exercise 8 The types of these functions are omitted as they are a part of the exercises

  def map[B](f: A => B): Stream[B] = 
    foldRight[Stream[B]] (Empty) { (a, acc) =>
      cons(f(a), acc)
    }

  def filter(f: A => Boolean): Stream[A] = 
    foldRight[Stream[A]] (Empty) { (a, acc) =>
      if (f(a)) cons(a, acc) else acc
    }

  def append[B>:A] (that: Stream[B]): Stream[B] = 
    foldRight(that) { (a, acc) =>
      cons(a, acc)
    }

  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight[Stream[B]] (Empty) { (a, acc) =>
      f(a).foldRight(acc) { (a, acc2) =>
        cons(a, acc2)
      }
    }

  //Exercise 09
  def find(p: A => Boolean): Option[A] = filter(p).headOption
  //Put your answer here:
  /*
  It is not optimal for list, as list would run though the entire collection when filtering
  whereas stream.filter makes the tail a function and thus only calls the tail when it is used.
  */

  // Exercise 13

  def map_[B](f: A => B): Stream[B] = unfold((headOption, tail)) { s =>
    s._1 match {
      case None => None
      case Some(a) => Some((f(a), (s._2.headOption, s._2.tail)))
    }
  }

  def take_(n: Int): Stream[A] = unfold((n, this)) { ns =>
    if (ns._1 <= 0) None else ns._2 match {
      case Empty => None
      case Cons(h,t) => Some((h(), (ns._1-1, t())))
    }
  }

  def takeWhile_(p: A => Boolean): Stream[A] = unfold(this) { s =>
    s match {
      case Empty => None
      case Cons(h,t) => if (p(h())) Some((h(), t())) else None
    }
  }

  def zipWith_[B,C] (f: (A,B) => C) (s: Stream[B]): Stream[C] = unfold((this, s)) { ts =>
    ts match {
      case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  }
}


case object Empty extends Stream[Nothing]
case class Cons[+A] (h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq


  // Exercise 1

  def from (n: Int): Stream[Int] = cons(n, from(n+1))

  def to (n: Int): Stream[Int] = cons(n, to(n-1))

  val naturals: Stream[Int] = from(1)

   //Exercise 10
  //Put your answer here:
  val fibs: Stream[Int] = {
    def aux(a:Int, b:Int): Stream[Int] = cons(a, aux(a+b, a))
    aux(0,1)
  }

  //Exercise 11
  /*
  def unfold [A, S] (z: S) (f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a,s)) => cons(a, unfold(s) (f))
  }*/

  def unfold [A, S] (z: S) (f: S => Option[(A, S)]): Stream[A] =
    f(z).map ((v: (A,S)) => cons(v._1, unfold (v._2) (f))).getOrElse(Empty)


  // Exercise 12

  def fib2: Stream[Int] = unfold((0,1)) { prev =>
    Some((prev._1, (prev._2, prev._1 + prev._2)))
  }

  def from2(n: Int): Stream[Int] = unfold(n) { n =>
    Some(n, n+1)
  }

  //tests

  //test:8    //see test file
  System.out.println("append")
  System.out.println(naturals.append (naturals))
}
