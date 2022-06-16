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
  //case 3: 1+2=3

  // Exercise 2

  def tail[A] (as: List[A]) :List[A] = as match {
    case Nil => throw new Exception ("")
    case Cons(_, t) => t
  }

  // Exercise 3

  @annotation.tailrec
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
  def drop[A] (l: List[A], n: Int) : List[A] = if (n<1) l else l match {
    case Nil => throw new Exception ("")
    case Cons(_, t) => drop(t, n-1)
  } 

  // Exercise 4

  def dropWhile[A] (l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h,t) => if(f(h)) dropWhile(t, f) else l
  } 

  // Exercise 5

  def init[A] (l: List[A]): List[A] = l match {
    case Nil => throw new Exception ("")
    case Cons(h, Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }
  //O(n)

  // Exercise 6

  def length[A] (as: List[A]): Int = 
    foldRight[A,Int] (as, 0)((_,acc)=>acc+1)

  // Exercise 7

  // @annotation.tailrec
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h,t)=>foldLeft[A,B](t, f(z,h)) (f)
  }


  // Exercise 8

  def product (as: List[Int]): Int = 
    foldLeft(as, 1) ((acc,v)=>acc*v)

  def length1[A] (as: List[A]): Int = 
    foldLeft(as, 0) ((acc,_)=>acc+1)

  // Exercise 9

  def reverse[A] (as: List[A]): List[A] = 
    foldLeft[A, List[A]] (as, Nil) ((acc,v)=>Cons(v,acc))


  // Exercise 10

  def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B): B = 
    foldLeft[A, B] (reverse(as), z) ((acc,v)=>f(v,acc))


  // Exercise 11

  def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B): B = 
    foldRight1[A,B=>B](as, (z1)=>z1) ((a,c) => (z1) => c(f(z1,a))) (z)
  //continuation

  // Exercise 12

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def concat[A] (as: List[List[A]]): List[A] = 
    foldLeft[List[A], List[A]](as, Nil) ((acc,v)=>append(acc,v))

  // Exercise 13

  def filter[A] (as: List[A]) (p: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h,t)=>if (p(h)) Cons(h, filter(t)(p)) else filter(t)(p)
  }

  // Exercise 14

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = 
    foldLeft[A, List[B]](as, Nil) ((acc,v)=>append(acc,f(v)))

  // Exercise 15

  def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] = 
    flatMap(l)(v=>if(p(v)) Cons(v,Nil) else Nil)

  // Exercise 16

  def add (l: List[Int]) (r: List[Int]): List[Int] = (l,r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, add(t1)(t2))
  }

  // Exercise 17

  def zipWith[A,B,C] (f: (A,B)=>C) (l: List[A], r: List[B]): List[C] = (l,r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(f)(t1,t2))
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

}
