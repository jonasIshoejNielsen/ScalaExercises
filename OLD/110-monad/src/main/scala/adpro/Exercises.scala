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
      (implicit arbA: Arbitrary[A]) = forAll{ (a1:A, a2:A) =>
        f(self.op(a1,a2)) shouldBe mb.op(f(a1), f(a2))   //distributive
        f(self.zero) shouldBe mb.zero                    //preserves identity
      }

    def isomorphism[B: Arbitrary] (f: A => B, g: B => A) (mb: Monoid[B])
      (implicit arbA: Arbitrary[A]) = forAll{ (a1:A, a2:A) =>
        f(self.op(a1,a2)) shouldBe mb.op(f(a1), f(a2))
        f(self.zero) shouldBe mb.zero

        val b1 = f(a1)
        val b2 = f(a2)
        
        g(mb.op(b1,b2)) shouldBe self.op(g(b1), g(b2))
        g(mb.zero) shouldBe self.zero
        
      }

    // Exercise 7 continues in MonoidExercisesSpec below

  } // Monoid.Laws

} // trait Monoid


object Monoid {

  val stringMonoid = new Monoid[String] {

    def op (a1: String, a2: String) =
      a1 + a2

    val zero = ""

  }



  def listMonoid[A] = new Monoid[List[A]] {

    def op (a1: List[A], a2: List[A]) =
      a1 ++ a2

    val zero = Nil

  }


  // Exercise 1

  lazy val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int) = x + y
    val zero = 0
  }


  lazy val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int) = x * y
    val zero = 1
  }


  lazy val booleanOr: Monoid[Boolean] = new Monoid[Boolean]{
    def op(x:Boolean, y:Boolean) = x || y
    val zero = false
  }


  lazy val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x && y
    val zero = true
  }


  // Exercise 2

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]): Option[A] = x match {case Some(x) => Some(x) case _ => y}
    val zero = None
  }


  // todo: Do we need to change method signature here?
  def optionMonoidLift[A: Monoid]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]): Option[A] = (x,y) match {
      case (Some(x), Some(y)) => Some(implicitly[Monoid[A]].op(x,y))
      case (Some(x), None) => Some(x)
      case (None, Some(y)) => Some(y)
      case _ => None
    }
    val zero = None
  }
  

  // Exercise 3

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(x: (A => A), y: (A => A)): A => A = a => y(x(a))
    var zero = identity
  }


  // Exercise 4 continues below in MonoidExercisesSpec

  // Exercise 5

  def foldMap[A,B: Monoid] (as: List[A]) (f: A => B): B = 
    as.foldRight[B](implicitly[Monoid[B]].zero)((a,acc)=> implicitly[Monoid[B]].op(f(a),acc))

  // Exercise 6 continues above in Monoid.Laws

  // Exercise 9

  def productMonoid[A,B] (ma: Monoid[A]) (mb: Monoid[B]): Monoid[(A,B)] = new Monoid[(A,B)] {
    def op(x:(A,B), y:(A,B)): (A,B) = (ma.op(x._1, y._1), mb.op(x._2, y._2))
    var zero = (ma.zero, mb.zero)
  }

} // object Monoid


class MonoidExercisesSpec
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers
  with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  val M = Monoid

  // Exercise 10

  "Exercise 10 (tests exercise 9, written by student)" - {

    "productMonoid (optionMonoid[Int]) (listMonoid[String]) gives a monoid" in
      (M.productMonoid (M.optionMonoid[Int]) (M.listMonoid[String])).Laws.monoid

  } // We continue with Exercise 10 below in Foldable


  // Exercise 8

  "Exercise 8 (tests Exercise 1, written by student)" - {

    "booleanOr is isomorphic to booleanAnd" in 
      Monoid.booleanOr.Laws.isomorphism[Boolean](!_, !_)(Monoid.booleanAnd)

  } // We continue with Exercise 9 above, in the Monoid object


  // Exercise 7

  "Exercise 7 (tests Exercise 6, written by student)" - {
    
    "stringMonoid is isomorphic to listMonoid[Char]" in 
      Monoid.stringMonoid.Laws.isomorphism[List[Char]](s => s.toList, lst => lst.mkString)(Monoid.listMonoid[Char])

  } // Exercise 8 continues above (tests are in the opposite order to
    // make test results from `sbt test` more readable)


  // Exercise 4

  "Exercise 4 (tests exercises 1-2, written by student)" - {

    "intAddition is a monoid" in Monoid.intAddition.Laws.monoid

    "intMultiplication is a monoid" in Monoid.intMultiplication.Laws.monoid

    "booleanOr is a monoid" in Monoid.booleanOr.Laws.monoid

    "booleanAnd is a monoid" in Monoid.booleanAnd.Laws.monoid

    "optionMonoid is a monoid" in Monoid.optionMonoid[Int].Laws.monoid

    "optionMonoidLift is a monoid" in Monoid.optionMonoidLift[Int](Monoid.intAddition).Laws.monoid

    // Exercise 5 continues above in the Monoid companion object

  }

}


trait Foldable[F[_]] {

  def foldRight[A,B] (as: F[A]) (z: B) (f: (A,B) => B): B

  def foldLeft[A,B] (as: F[A]) (z: B) (f: (B,A) => B): B

  def foldMap[A,B] (as: F[A]) (f: A => B) (mb: Monoid[B]): B

  def concatenate[A] (as: F[A]) (m: Monoid[A]): A =
    foldLeft (as) (m.zero) (m.op)

  // Exercise 12 

  def toList[A] (fa: F[A]): List[A] = 
    foldRight[A,List[A]] (fa) (Nil) ((a,acc)=>a::acc)

  // Continue with Exercise 13 below Foldable

}


object Foldable {

  // Exercise 11

  def foldableList[A]: Foldable[List] = new Foldable[List] {
    
    def foldRight[A,B] (as: List[A]) (z: B) (f: (A,B) => B): B = as.foldRight(z)(f)

    def foldLeft[A,B] (as: List[A]) (z: B) (f: (B,A) => B): B = as.foldLeft(z)(f)

    def foldMap[A,B] (as: List[A]) (f: A => B) (mb: Monoid[B]): B = as.foldRight(mb.zero)((a,acc) => mb.op(f(a),acc))

  }

  // see Exercise 12 just above

}


trait Functor[F[_]] { self =>

  def map[A,B] (fa: F[A]) (f: A => B) :F[B]

  object Laws {

    def map[A] (implicit arbFA: Arbitrary[F[A]]) =
      forAll { fa: F[A] =>
        self.map[A, A] (fa) (identity[A]) should
          be (fa)
      }
  }

}

object Functor {

  val listFunctor =
    new Functor[List] {

      def map[A,B] (as: List[A]) (f: A => B): List[B] =
        as map f
    }

  // Exercise 13

  lazy val optionFunctor: Functor[Option] = new Functor[Option]{
    def map[A,B](as: Option[A])(f: A => B): Option[B] = 
      as map f
  }

  // Exercise 14 continues directly below in the Spec class

}

class FunctorExercisesSpec
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers
  with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  val F = Functor

  "Exercise 14 (listFunctor and optionFunctor)" - {

    "listFunctor satisfies the map law" in
      F.listFunctor.Laws.map[Int]

    // Exercise 14

    "optionFunctor satisfies map law (tests Exercise 13, written by student)" in
      F.optionFunctor.Laws.map[Int]

  }

  // Exercise 15 continues below the Monad trait, in the Monad companion object

}


trait Monad[F[_]] extends Functor[F] { self =>

  def unit[A]  (a: => A): F[A]

  def flatMap[A,B] (ma: F[A]) (f: A => F[B]): F[B]

  def map[A,B] (ma: F[A]) (f: A => B): F[B] =
    flatMap (ma) (a => unit (f (a)))

  def map2[A, B, C] (ma: F[A], mb: F[B]) (f: (A,B) => C): F[C] =
    flatMap (ma) (a => map (mb) (b => f (a, b)))


  // Exercise 17

  def sequence[A] (lfa: List[F[A]]): F[List[A]] = 
    lfa.foldRight[F[List[A]]] (unit(Nil)) ((a,acc) => map2(a,acc)((a,lst) => a::lst) )

  // Exercise 18

  def replicateM[A] (n: Int, ma: F[A]): F[List[A]] = 
    sequence(List.fill(n)(ma))

  // Write in the comment here ...
  // if option:  Some list of values, unless one or more is None, then it will return None
  // if list: return list of list of values
  // convert one value to multiple all wrapped in same wrapper


  // Exercise 19

  def compose[A,B,C] (f: A => F[B], g: B => F[C]): A => F[C] = a =>
    flatMap (f(a)) (b => g(b))

  object MonadLaws {

    def associative[A, B, C]
      (implicit arbFA: Arbitrary[F[A]],
               arbAFB: Arbitrary[A => F[B]],
               arbBFC: Arbitrary[B => F[C]]) =
      forAll { (x: F[A], f: A => F[B], g: B => F[C]) =>
        val left = self.flatMap (self.flatMap (x) (f)) (g)
        val right = self.flatMap (x) (a => self.flatMap (f (a)) (g))
        left should be (right)
      }


    def identityRight[A]
      (implicit arbFA: Arbitrary[F[A]], arbAFA: Arbitrary[A => F[A]]) =
      forAll { (x: F[A], f: A => F[A]) =>
        self.flatMap[A,A] (x) (a => self.unit[A] (a)) should be (x) }


    def identityLeft[A: Arbitrary] (implicit arbAFA: Arbitrary[A => F[A]]) =
      forAll { (y :A, f: A => F[A]) =>
        self.flatMap[A,A] (self.unit[A](y)) (f) should be (f(y)) }


    def identity[A: Arbitrary]
      (implicit arbFA: Arbitrary[F[A]], arbAFA: Arbitrary[A => F[A]]) = {
      withClue ("identity left: ") { identityLeft[A]  }
      withClue ("identity right:") { identityRight[A] }
    }


    def monad[A: Arbitrary, B, C]
      (implicit arbFA: Arbitrary[F[A]],
               arbAFA: Arbitrary[A => F[A]],
               arbAFB: Arbitrary[A => F[B]],
               arbBFC: Arbitrary[B => F[C]]) = {
        withClue ("associative:") { self.MonadLaws.associative[A,B,C] }
        withClue ("identity:") { self.MonadLaws.identity[A] }
      }

  }

}

object Monad {

  // Exercise 15

  lazy val optionMonad: Monad[Option] = new Monad[Option]{
    def unit[A] (a: => A): Option[A] = Some(a)

    def flatMap[A,B] (ma: Option[A]) (f: A => Option[B]): Option[B] =
      ma flatMap f
  }


  lazy val listMonad: Monad[List] = new Monad[List] {
    def unit[A] (a: => A): List[A] = List(a)

    def flatMap[A,B] (ma: List[A]) (f: A => List[B]): List[B] =
      ma flatMap f
  }

  // Exercise 16 continues directly below in MonadExercisesSpec
}

class MonadExercisesSpec
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers
  with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  val M = Monad

  // Exercise 16

  "Exercise 16 (tests Exercise 15, written by student)" - {

    "optionMonad is a monad" in 
      M.optionMonad.MonadLaws.monad[Int, Int, Int]

    "listMonad is a monad" in 
      M.listMonad.MonadLaws.monad[Int, Int, Int]
  }

  // Exercise 17 continues above in the Monad trait

}
