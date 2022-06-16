// Advanced Programming, Andrzej Wasowski, IT University of Copenhagen
package adpro

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import Reduce._

class ReduceSpec
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers
  with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
  with org.scalatest.prop.TableDrivenPropertyChecks {

  "Exercise 8 (toList)" - {

    "Reduce[List[Int]].toList is identity" in {
      implicit val monoid = adpro.Monoid.intAddition
      forAll { l: List[Int] =>
        reduce[List].toList[Int] (l) should be (l)
      }
    }

  }

  "Excercise 7 (reduceList)" - {

    "Reduce[List] reducers collapse for monoids Int+" in {
      implicit val monoid = adpro.Monoid.intAddition
      reduce[List].Laws.reducersEquivalentForMonoids[Int]
    }

    "Reduce[List] reducers collapse for monoid Int*" in {
      implicit val monoid = adpro.Monoid.intMultiplication
      reduce[List].Laws.reducersEquivalentForMonoids[Int]
    }

    "Reduce[List] reducers collapse for string monoid" in {
      implicit val monoid = adpro.Monoid.stringMonoid
      reduce[List].Laws.reducersEquivalentForMonoids[String]
    }

    "Reduce[List] reducers collapse for list monoid" in {
      implicit val monoid = adpro.Monoid.listMonoid[Boolean]
      reduce[List].Laws.reducersEquivalentForMonoids[List[Boolean]]
    }
  }

}


