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

// The extension of App allows writing the main method statements at the top
// level (the so called default constructor). For App objects they will be
// executed as if they were placed in the main method in Java.

package fpinscala

object Exercises extends App with ExercisesInterface {

  import fpinscala.List._

  // Exercise 3

  //def fib (n: Int): Int = fib (n-2) + fib (n-1)
  def fib (n: Int): Int = {
    @annotation.tailrec
    def aux (n:Int, a:Int, b:Int):Int = {
      if (n<=1) a
      else aux(n-1, a+b, a)
    }
    aux(n, 0, 1)
  } 

  // Exercise 4

  def isSorted[A] (as: Array[A], comparison: (A,A) =>  Boolean): Boolean = {
    def aux(i1:Int, i2:Int):Boolean={
      if (i2>=as.length) true
      else if (!comparison(as(i1), as(i2))) false
      else aux(i1+1, i2+1)
    }
    aux(0,1)
  }

  // Exercise 5

  def curry[A,B,C] (f: (A,B)=>C): A => (B => C) = 
    (a:A) => (b:B) => f(a,b) 

  def isSortedCurry[A] :Array[A] =>((A,A) =>Boolean) =>Boolean = {
    //curry[Array[A], (A,A) =>  Boolean, Boolean] ((as,c)=> isSorted(as, c))
    curry ((as,c)=> isSorted(as, c))
  }

  // Exercise 6

  def uncurry[A,B,C] (f: A => B => C): (A,B) => C = 
    (a,b) => f (a) (b)

  def isSortedUnCurry[A] : (Array[A], ((A,A) =>Boolean)) =>Boolean = {
    uncurry(isSortedCurry)
  }

  // Exercise 7

  def compose[A,B,C] (f: B => C, g: A => B) : A => C = 
    a => f(g(a))

}
