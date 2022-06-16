// Advanced Programming
// Andrzej Wasowski, IT University of Copenhagen

// NOTE: It is exected that two tests fail (one for l1 and one for l2) If you
// uncommend the second test for l3, it will fail, too (by design). The other
// tests should pass.

package adpro

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import monocle._
import monocle.syntax.apply._
import monocle.{Lens, Optional}

class LensesSpec
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers
  with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
  with org.scalatest.prop.TableDrivenPropertyChecks {


    implicit val arbAddress =
      Arbitrary (Gen.resultOf (Lenses.Address))

    implicit val arbUniversity =
      Arbitrary (Gen.resultOf (Lenses.University))


    "Exercise 12: using 'ith'" - {

      "List (1..6)(3)++" in {
        Lenses.list1 should be (List (1,2,4,4,5,6))
      }

    }


    "Exercise 11: ith" - {

      "partial get success (ith)" in {
        forAll { l: List[Int] =>
          whenever (!l.isEmpty) {
            forAll (Gen.choose (0, l.size-1)) {
              n: Int =>
                Lenses.ith[Int] (n).getOption (l) should
                  be (Some (l (n)))
            }
          }
        }
      }

      "partial get failure (ith)" in {
        forAll { l: List[Int] =>
          whenever (!l.isEmpty) {
            forAll (Gen.choose (l.size, l.size + 100)) {
              n: Int =>
                Lenses.ith[Int] (n).getOption (l) should
                  be (None)
            }
          }
        }
      }

      "partial replace success (ith)" in {
        forAll { l: List[Int] =>
          whenever (!l.isEmpty) {
            forAll (Gen.choose (0, l.size-1)) {
              n: Int =>
                Lenses.ith[Int] (n).replace (42) (l) should
                  be (l.updated (n, 42))
            }
          }
        }
      }

      "partial replace failure (ith)" in {
        forAll { l: List[Int] =>
          whenever (!l.isEmpty) {
            forAll (Gen.choose (l.size, l.size + 100)) {
              n: Int =>
                Lenses.ith[Int] (n).replace (42) (l) should
                  be (l)
            }
          }
        }
      }

      "ith1 is well behaved (total)" in {
        forAll (Gen.choose (0, 10000)) {
          n: Int =>
            Lenses.ith1[Int] (42) (n+3).get (List (1,2,3)) should
              be (42)
            Lenses.ith1[Int] (42) (n).replace (42) (Nil) should
              be (List.fill (n+1) (42))
        }
      }

    }


    "Exercise 10: capitalizing selected countries" - {

      "itu6 should have selected countries in upper case" in {
        Lenses.itu6
          .students
          .filter { case (s,a) => s(0) == 'A' }
          .values
          .map {_.country}
          .foreach { s => s.toUpperCase should be (s) }
      }

      "The remaining countries in itu6 are unchanged" in {
        Lenses.itu6
          .students
          .filter { case (s,a) => s(0) != 'A' }
          .keys
          .foreach { s =>
            Lenses.itu6.students.get (s) should
              be (Lenses.itu.students.get (s))
          }
      }
    }


    "Exercise 9: capitalizing countries" - {

      "itu5 should have all the countries in upper case" in {
        Lenses.itu5
          .students
          .values
          .map {_.country}
          .foreach { s => s.toUpperCase should be (s) }
      }

    }


    "Exercise 8: lenses for the university with macros" - {

      "_zipcode1 is total and very well behaved" in {
        Lenses.Laws.veryWellBehavedTotalLense (Lenses._zipcode1)
      }

      "_students1 is very well behaved" in {
        Lenses.Laws.veryWellBehavedTotalLense (Lenses._students1)
      }

      "itu3 should show Alex at zipcode 9100" in {
        Lenses.itu3.students ("Alex").zipcode should
          be ("9100")
      }

      "itu4 should show Alex at zipcode 9100" in {
        Lenses.itu4.students ("Alex").zipcode should
          be ("9100")
      }
    }


    "Exercise 7: lenses for the university" - {

      "_zipcode is total and very well behaved" in {
        Lenses.Laws.veryWellBehavedTotalLense (Lenses._zipcode)
      }

      "_students is very well behaved" in {
        Lenses.Laws.veryWellBehavedTotalLense (Lenses._students)
      }

      "itu2 should show Alex at zipcode 9100" in {
        Lenses.itu2.students ("Alex").zipcode should
          be ("9100")
      }
    }


    "Exercise 6: itu1, moving Alex to 9100" - {

      "itu should show Alex at zipcode 2800" in {
        Lenses.itu.students ("Alex").zipcode should
          be ("2800")
      }

      "itu1 should show Alex at zipcode 9100" in {
        Lenses.itu1.students ("Alex").zipcode should
          be ("9100")
      }

      "itu1 should be the same as itu for other students than Alex" in {

        val students =
           Table[Lenses.Name] (
             heading = "student name",
             rows = Lenses.itu.students.keySet.toSeq: _*
           )

        forAll (students) { name: Lenses.Name =>
          whenever (name != "Alex") {
             Lenses.itu1.students (name).zipcode should
               be (Lenses.itu.students (name).zipcode)
          }
        }
      }

    }


    "Exercise 5 (codiag1)" in {
      Lenses.Laws.veryWellBehavedTotalLense (Lenses.codiag1[Int])
      Lenses.Laws.veryWellBehavedTotalLense (Lenses.codiag1[String])
    }

    "Exercise 4 (codiag)" in {
      Lenses.Laws.veryWellBehavedTotalLense (Lenses.codiag[Int])
      Lenses.Laws.veryWellBehavedTotalLense (Lenses.codiag[String])
    }

}

class ImportExercise3Spec extends Lenses.Exercise3Spec
