// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: _____
//
// AUTHOR1: __________
// TIME1: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: __________
// TIME2: _____ <- how much time have you used on solving this exercise set
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
// val q = new java.awt.Point(0,2) with OrderedPoint
// assert(p < q)

sealed trait Tree[+A]
case class Leaf[A] (value: A) extends Tree[A]
case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 2

  def size[A] (t: Tree[A]): Int = t match {
    case Leaf(_)=>1
    case Branch(l,r)=>1+size(l)+size(r)
  }

  // Exercise 3

  def maximum (t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  // Exercise 4

  def map[A,B] (t: Tree[A]) (f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // Exercise 5

  def fold[A,B] (t: Tree[A]) (f: (B,B) => B) (g: A => B): B = t match {
    case Leaf(value) => g(value)
    case Branch(left, right) => f(fold(left)(f)(g), fold(right)(f)(g))
  }

  def size1[A] (t: Tree[A]): Int = 
    fold[A,Int](t)((v1,v2)=>1+v1+v2)(_=>1)

  def maximum1 (t: Tree[Int]): Int = 
    fold[Int,Int](t)(_ max _)(v=>v)

  def map1[A,B] (t: Tree[A]) (f: A=>B): Tree[B] =
    fold[A, Tree[B]](t)((l,r)=>Branch(l,r))(v=>Leaf(f(v)))

}

sealed trait Option[+A] {

  // Exercise 6

  def map[B] (f: A=>B): Option[B] = this match {
    case None => None
    case Some(v)=>Some(f(v))
  }

  /**
   * Ignore the arrow (=>) in default's type below for now.
   * It prevents the argument "default" from being evaluated until it is needed.
   * So it is not evaluated if we process a Some object (this is 'call-by-name'
   * and we should talk about this soon).
   */

  def getOrElse[B >: A] (default: => B): B = this match {
    case None => default
    case Some(get) => get
  }

  def flatMap[B] (f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(get) => f(get)
  }

  def filter (p: A => Boolean): Option[A] = this match {
    case None => None
    case Some(get) => if (p(get)) this else None
  }

}

case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]

object ExercisesOption {

  // mean is implemented in Chapter 4 of the text book

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some (xs.sum / xs.length)

  // Exercise 7

  def headOption[A] (lst: List[A]): Option[A] = lst match {
    case Nil => None
    case h ::t => Some (h)
  }

  //def headGrade (lst: List[(String,Int)]): Option[Int] = 
  //  headOption(lst).map(_._2)

  def headGrade (lst: List[(String,Int)]): Option[Int] = for{
    x1 <- headOption(lst)
    x2 = x1._2
  } yield x2

  // Exercise 8
  /*
  def variance (xs: Seq[Double]): Option[Double] = for {
    m <- mean(xs)
    v <- mean(xs.map(a => math.pow(a - m, 2)))
  } yield v
  */
  def variance (xs: Seq[Double]): Option[Double] = 
    mean(xs).flatMap(m=>mean(xs.map(x=>math.pow(x - m, 2))))


  // Exercise 9

  def map2[A,B,C] (ao: Option[A], bo: Option[B]) (f: (A,B) => C): Option[C] = 
    ao.flatMap((v1)=>bo.map((v2)=>f(v1,v2)))
  /*
  def map2[A,B,C] (ao: Option[A], bo: Option[B]) (f: (A,B) => C): Option[C] = for {
    a <- ao
    b <- bo
  } yield f(a, b)
  */

  // Exercise 10

  def sequence[A] (aos: List[Option[A]]): Option[List[A]] = 
    aos.foldRight[Option[List[A]]] (Some(List())) { (a, acc) =>
      map2(acc, a) ((lst, e) => e :: lst)
    }

  // Exercise 11

  def traverse[A,B] (as: List[A]) (f: A => Option[B]): Option[List[B]] = 
    as.foldRight[Option[List[B]]] (Some(List())) { (a, acc) =>
      acc.flatMap(lst => f(a).map(_ :: lst))
    }

}
