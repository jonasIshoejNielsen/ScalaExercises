// Andrzej Wąsowski, Advanced Programming
// based on fpinscala exercises
package adpro

import scala.language.higherKinds

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._
import org.scalatest.matchers.should.Matchers._
import org.scalactic.Equality

trait Monoid[A] { self =>

  def op (a1: A, a2: A): A
  def zero: A

  // Some Laws for monoids (We place them here like in Parsers)

  object Laws {

    // The instance of Eqaulity type class will be used for equality
    // comparisons.  Normally the default equality is fine, but we need to
    // parameterize to handle endoMonoid (comparisons of functions).  See more
    // comments about that in MonoidSpec.scala if interested.

    def associative (implicit arbA: Arbitrary[A], eqA: Equality[A]) =
      forAll { (a1: A, a2: A, a3: A) =>
        self.op (self.op (a1, a2), a3) should === { self.op (a1, self.op (a2, a3)) }
      }


    // Scalatest property based testing is imperative (its matchers are
    // assertions that fail imperatively, not Props that return a result, a
    // failure or success).  Scalatest allows to use Scalacheck's API directly
    // which is pure and very similar to the book.  I chose not to use becase it
    // seems that the documentation is better for the imperative interface, so
    // all exercises in the course requiring scalatest became easier this way.
    // Unfortunately, it has negative consequences as well.  We cannot use
    // logical operators (esp.  conjunction) to create more complex laws from
    // simpler ones.  This has to be done by more ad hoc means, as seen below.
    //
    // To work around this I turned "forAll ( x )  && forAll ( y )" into "forall
    // ( x; y)", which fortunately means the same logically, but in imperative
    // style.  This looses the logical style of specifications a bit. Also,
    // incidentally, it also seems to make it impossible to mark subexpressions
    // with labels, like the book does. If you are lost by this discussion, then
    // ignore it.  This is a discussion that the authors' of our textbook would
    // likely enjoy. If you do appreciate it too, then your chances to ace the
    // course have just grown drammatically.

    def unit (implicit arbA: Arbitrary[A], eqA: Equality[A]) =
      forAll { a: A =>
        self.op (a, self.zero) should === (a)
        self.op (self.zero, a) should === (a)
      }

    // This law has the same problem as unit above.  it should really be
    // monoid = associative && unit, but using the imperative API of Scalatest
    // needs to be turned into monoid (m) = { associative (m); unit (m) }

    def monoid (implicit arbA: Arbitrary[A], eqA: Equality[A]) = {
      associative
      unit
    }

    // Exercise 6

    def homomorphism[B] (f: A => B) (mb: Monoid[B])
      (implicit arbA: Arbitrary[A]) = {
        forAll { (a1: A, a2: A) =>
          mb.op (f(a1), f(a2)) should
            be { f (self.op (a1,a2)) }
        }
        f (self.zero) should be (mb.zero)
    }

    // Again Scala matchers cannot be conjoined so we need to just sequence
    // the side-effects. Yuck!

    def isomorphism[B: Arbitrary] (f: A => B, g: B => A) (mb: Monoid[B])
      (implicit arbA: Arbitrary[A]) = {
        self.Laws.homomorphism (f) (mb)
        mb.Laws.homomorphism (g) (self)
      }

    // Exercise 7 continues in MonoidExercisesSpec below

  } // Monoid.Laws

} // trait Monoid


object Monoid {

  // A helper function to access implicit monoids nicely
  def monoid[A: Monoid]: Monoid[A] =
    implicitly[Monoid[A]]

  val stringMonoid = new Monoid[String] {

    def op (a1: String, a2: String) =
      a1 + a2

    val zero = ""

  }



  def listMonoid[A] = new Monoid[List[A]] {

    def op (a1: List[A], a2: List[A]) =
      a1 ++ a2

    val zero = scala.collection.immutable.Nil

  }


  // Exercise 1
  lazy val intAddition: Monoid[Int] = new Monoid[Int] {

    def op (a1: Int, a2: Int) =
      a1 + a2

    val zero = 0

  }


  lazy val intMultiplication: Monoid[Int] = new Monoid[Int] {

    def op(a1: Int, a2: Int) = a1 * a2

    val zero = 1

  }


  lazy val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {

    def op (a1: Boolean, a2: Boolean) =
      a1 || a2

    val zero = false

  }


  lazy val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {

    def op (a1: Boolean, a2: Boolean) =
      a1 && a2

    val zero = true

  }

  // Exercise 2

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {

    def op (a1: Option[A], a2: Option[A]): Option[A]=
      a1 orElse a2

    val zero = None

  }


  def optionMonoidLift[A: Monoid]: Monoid[Option[A]] = new Monoid[Option[A]] {

    def op (a1: Option[A], a2: Option[A]) =
      (a1,a2) match {

        case (None,None) =>
          None
        case (Some (a),None) =>
          a1
        case (None, Some (a)) =>
          a2
        case (Some (a1), Some (a2)) =>
          Some (monoid[A].op (a1,a2))
      }

    val zero = None

  }


  // Exercise 3

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {

    def op (f: A => A, g: A => A) =
      f compose g

    val zero = identity[A]

  }

  // Exercise 4 continues below in MonoidExercisesSpec

  // Exercise 5

  def foldMap[A,B: Monoid] (as: List[A]) (f: A => B): B = {
    val m = monoid[B]
    as.foldLeft (m.zero) ((b,a) => m.op (b, f(a)))
  }

  // Exercise 6 continues above in Monoid.Laws

  // Exercise 9

  def productMonoid[A,B] (ma: Monoid[A]) (mb: Monoid[B]) =
    new Monoid[(A,B)] {

      def op (ab1: (A,B), ab2: (A,B)) =
        ma.op (ab1._1, ab2._1) -> mb.op (ab1._2, ab2._2)

      val zero = ma.zero -> mb.zero
  }

} // object Monoid
