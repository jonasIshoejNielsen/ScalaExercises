// Advanced Programming, Wasowski, IT University of Copenhagen
package adpro

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._
import org.scalatest.matchers.should.Matchers._

import adpro.Monoid.monoid

/**
 * The type class for reducible structures.  I changed the type of reducers to
 * not use curried operators, but regular binary operators.  This is more in
 * line with the types give to folds in Scala, and gives easier to read syntax
 * of expressions.  (Curried style is preferred in Haskell.)
 **/

trait Reduce[F[_]] { self =>

  def reduceR[A, B] (opr: (A,B) => B) (fa: F[A], b: B): B

  def reduceL[A, B] (opl: (B,A) => B) (b: B, fa: F[A]): B



  // Exercise 8

  def toList[A] (fa: F[A]): List[A] = 
    reduceR[A,List[A]] ((a,acc) => a::acc) (fa, List())

  object Laws {

    def reducersEquivalentForMonoids[A: Monoid]
      (implicit arbFA: Arbitrary[F[A]]) =
        forAll { fa: F[A] =>
          val M = monoid[A]
          val a = self.reduceR[A,A] (M.op _) (fa, M.zero)
          val b = (self.reduceL[A,A] (M.op _) (M.zero , fa))
          a shouldBe b
        }
  }

}

object Reduce {

  // A helper function allowing to find implicit instances of Reduce nicely

  def reduce[F[_]: Reduce]: Reduce[F] =
    implicitly[Reduce[F]]

  // Exercise 7

  implicit lazy val reduceList: Reduce[List] = new Reduce[List] {
    def reduceR[A, B] (opr: (A,B) => B) (fa: List[A], b: B): B = 
      fa.foldRight[B](b)((a,acc)=>opr(a,acc))

    def reduceL[A, B] (opl: (B,A) => B) (b: B, fa: List[A]): B = 
      fa.foldLeft[B](b)((acc,a)=>opl(acc,a))
  }
}
