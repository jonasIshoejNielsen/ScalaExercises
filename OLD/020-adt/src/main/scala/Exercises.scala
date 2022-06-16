// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// Work on this file by following the associated exercise sheet
// (available in PDF in the same directory).
//
// The file is meant to be compiled inside sbt, using the 'compile' command.
// To run the compiled file use the 'run' or 'runMain'.
// To load the file int the REPL use the 'console' command.
//
// Continue solving exercises in the order presented in the PDF file. The file
// shall always compile, run, and pass tests (for the solved exercises), 
// after you are done with each exercise (if you do them in order).  
// Compile and test frequently. Best continously.

package fpinscala

object Exercises extends App with ExercisesInterface {

  import fpinscala.List._

  // Exercise 1 requires no programming
  //1+2=3


  // Exercise 2

  def tail[A] (as: List[A]) :List[A] = as match {
    case Cons(_, t) => t
    case Nil => throw new Exception ("")
  }

  // Exercise 3

  // @annotation.tailrec
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
  def drop[A] (l: List[A], n: Int) : List[A] = (n,l) match {
    case (0, _) => l
    case (_, Nil) => throw new Exception ("")
    case (_, Cons(_,t)) => drop(t,n-1)
  }

  // Exercise 4

  def dropWhile[A] (l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h,t) => if(f(h)) dropWhile(t, f) else l
    case Nil => Nil
  } 

  // Exercise 5

  def init[A] (l: List[A]): List[A] = l match {
    case Nil => throw new Exception("")
    case Cons(h, Nil) => Nil
    case Cons(h,t) => Cons(h, init(t))
  }
  //not constant, O(n)

  // Exercise 6

  def length[A] (as: List[A]): Int = 
    foldRight(as, 0) ((_, acc) => acc+1)

  // Exercise 7

  @annotation.tailrec
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B): B = as match {
    case Nil       => z
    case Cons(h,t) => foldLeft(t, f(z,h)) (f)
  }

  // Exercise 8

  def product (as: List[Int]): Int =
    foldLeft(as, 1) (_*_)
  
  def length1[A] (as: List[A]): Int = 
    foldLeft(as, 0) ((acc, _) => acc+1)

  // Exercise 9

  def reverse[A] (as: List[A]): List[A] =
    foldLeft(as, Nil: List[A]) ((acc, a) => Cons(a, acc))

  // Exercise 10

  def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B): B = 
    foldLeft(reverse(as), z) ((acc,a)=> f(a,acc))

  // Exercise 11
  def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B): B =
    foldRight(as, (z1:B) => z1) ((a,c) => (z1) => c(f(z1,a))) (z)

  // Exercise 12

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def concat[A] (as: List[List[A]]): List[A] = 
    foldRight1(as, Nil:List[A]) ((a, acc) => append (a, acc))

  // Exercise 13

  def filter[A] (as: List[A]) (f: A => Boolean): List[A] = 
    foldRight1(as, Nil:List[A]) ((a, acc)=> if (f(a)) Cons(a,acc) else acc)

  // Exercise 14

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = 
    concat(foldRight1(as, Nil:List[List[B]]) ((a,acc) => Cons(f(a), acc)))
    
  // Exercise 15

  def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] =
    flatMap(l) (a => if (p(a)) Cons(a, Nil) else Nil)

  // Exercise 16

  def add (l: List[Int]) (r: List[Int]): List[Int] = (l ,r) match {
    case (Cons(a, t1), Cons(b, t2)) => Cons(a+b, add(t1)(t2))
    case (_, _) => Nil
  }
  
  // Exercise 17

  def zipWith[A,B,C] (f: (A,B)=>C) (l: List[A], r: List[B]): List[C] = (l ,r) match {
    case (Cons(a, t1), Cons(b, t2)) => Cons(f(a,b), zipWith(f)(t1,t2))
    case (_, _) => Nil
  }

  // Exercise 18

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean = {
    def inner (lst1:List[A]) (lst2:List[A]):Boolean = (lst1, lst2) match{
      case (_,Nil)=> true
      case (Cons(h1,t1), Cons(h2,t2)) =>
        if (h1==h2)
          if(inner(t1)(t2)) true 
          else inner(t1)(sub)
        else inner(t1)(sub)
      case _ => false
    }
    inner(sup)(sub)
  }

  // Exercise 19

  def pascal (n: Int): List[Int] = {
    def convolve(l: List[Int]) (acc: List[Int]): List[Int] = l match {
      case Cons(a, Cons(b, t)) => convolve(drop(l, 1)) (Cons(a+b, acc))
      case _ => Cons(1, acc)
    }
    if (n == 1) List(1)
    else convolve(pascal(n-1)) (List(1))
  }
}
