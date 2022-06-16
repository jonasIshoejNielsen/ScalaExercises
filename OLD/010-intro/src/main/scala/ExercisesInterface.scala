// An abstract type for exercise to facilitate our testing setup
// The file also contains the definitions of basic types (List)

package fpinscala

// An ADT of Lists
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  // override function application to provide a factory of lists (convenience)

  def apply[A](as: A*): List[A] = // Variadic function
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B] (as :List[A], z: B) (f : (A,B)=> B) :B = as match {
    case Nil => z
    case Cons (x,xs) => f (x, foldRight (xs,z) (f))
  }
}

trait ExercisesInterface {

  def fib (n: Int): Int

  def isSorted[A] (as: Array[A], ordered: (A,A) =>  Boolean) :Boolean

  def curry[A,B,C] (f: (A,B) => C): A => (B => C)

  def uncurry[A,B,C] (f: A => B => C): (A,B) => C

  def compose[A,B,C] (f: B => C, g: A => B): A => C

}
