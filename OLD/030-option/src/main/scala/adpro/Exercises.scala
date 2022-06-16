// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: Join F# in august
//
// AUTHOR1: Jonas Ishøj Nielsen
// TIME1: 2 hours, 30 min <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: August Wester
// TIME2: 2 hours, 30 min <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR3: Anders Degn Lapiki
// TIME3: 2 hours, 30 min <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file shall always compile and run after you are done with each exercise
// (if you do them in order).  Please compile and test frequently. Of course,
// some tests will be failing until you finish. Only hand in a solution that
// compiles and where tests pass for all parts that you finished.  The tests
// will fail for unfnished parts.  Comment such out.

package adpro

// Exercise  1

trait OrderedPoint extends scala.math.Ordered[java.awt.Point] {

  this: java.awt.Point =>

  override def compare (that: java.awt.Point): Int =
    if ((this.x < that.x) || (this.x == that.x && this.y < that.y)) -1
    else if (this.x == that.x && this.y == that.y) 0
    else 1
}

// Try the following (and similar) tests in the repl (sbt console):
// val p = new java.awt.Point(0,1) with OrderedPoint
    // val p: java.awt.Point with OrderedPoint = $anon$1[x=0,y=1]
// val q = new java.awt.Point(0,2) with OrderedPoint
    //val q: java.awt.Point with OrderedPoint = $anon$1[x=0,y=2]
// assert(p < q)
    //()   //it doesn't fail

sealed trait Tree[+A]
case class Leaf[A] (value: A) extends Tree[A]
case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 2

  def size[A] (t :Tree[A]): Int = {
    def aux(t: Tree[A], n: Int): Int = t match {
      case Leaf(v)      => n + 1
      case Branch(l, r) => aux(l, n+1) + aux(r, 0)
    }
    aux(t, 0)
  }
  
  // Exercise 3

  def maximum (t: Tree[Int]): Int = {
    def aux(t: Tree[Int]): Int = t match {
      case Leaf(v)      => v
      case Branch(l, r) => aux(l).max(aux(r))
    }
    aux(t)
  }

  // Exercise 4

  def map[A,B] (t: Tree[A]) (f: A => B): Tree[B] = {
    def aux(t: Tree[A]): Tree[B] = t match {
      case Leaf(v)      => Leaf(f(v))
      case Branch(l, r) => Branch(aux(l), aux(r))
    }
    aux(t)
  }
  // Exercise 5

  def fold[A,B] (t: Tree[A]) (f: (B,B) => B) (g: A => B): B = {
    def aux(t: Tree[A]): B = t match {
      case Leaf(v) => g(v)
      case Branch(l, r) => f(aux(l), aux(r))
    }
    aux(t)
  }

  def size1[A] (t: Tree[A]): Int = 
    fold(t)((b1: Int, b2: Int) => b1 + b2 + 1) (_ => 1)

  def maximum1 (t: Tree[Int]): Int =
    fold(t)((b1: Int, b2: Int) => b1.max(b2)) (x => x)

  def map1[A,B] (t: Tree[A]) (f: A=>B): Tree[B] = 
    fold[A,Tree[B]] (t) ((l, r) => Branch(l, r)) (a => Leaf(f(a)))
}

sealed trait Option[+A] {

  // Exercise 6

  def map[B] (f: A=>B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }


  def getOrElse[B >: A] (default: => B): B = this match {
    case Some(v) => v
    case None => default
  }

  def flatMap[B] (f: A => Option[B]): Option[B] = this match {
    case Some(v) => f(v)
    case None => None
  }

  def filter (p: A => Boolean): Option[A] = this match {
    case Some(v) => if (p(v)) this else None
    case None => None
  }
}

case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]

object ExercisesOption {

  // mean is implemented in Chapter 4 of the text book

  def mean(xs: Seq[Double]): Option[Double] = 
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 7

  def variance (xs: Seq[Double]): Option[Double] = for {
    m <- mean(xs)
    v <- mean(xs.map(a => math.pow(a - m, 2)))
  } yield v

  // Exercise 8

  def map2[A,B,C] (ao: Option[A], bo: Option[B]) (f: (A,B) => C): Option[C] = for {
    a <- ao
    b <- bo
  } yield f(a, b)
  
  // Exercise 9

  def sequence[A] (aos: List[Option[A]]): Option[List[A]] =
    aos.foldRight[Option[List[A]]] (Some(List())) { (a, acc) =>
      map2(acc, a) ((lst, e) => e :: lst)
    }

  // Exercise 10

  def traverse[A,B] (as: List[A]) (f :A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]] (Some(List())) { (a, acc) =>
      acc.flatMap(lst => f(a).map(_ :: lst))
    }
}
