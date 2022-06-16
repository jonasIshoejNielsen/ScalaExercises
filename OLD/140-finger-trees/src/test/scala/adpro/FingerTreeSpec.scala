// Advanced Programming
// Andrzej Wasowski, IT University of Copenhagen

package adpro

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

class FingerTreeSpec
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers
  with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
  with org.scalatest.prop.TableDrivenPropertyChecks {

  import FingerTree._
  import Reduce._

  // Exercise 16 (Generators to be written)

  def fingerTreeOfN[A: Arbitrary] (n: Int): Gen[FingerTree[A]] = 
    for {
      la <- Gen.listOfN(n, Arbitrary.arbitrary[A])
    } yield FingerTree.toTree(la.take(n))


  def fingerTree[A: Arbitrary]: Gen[FingerTree[A]] =  
    for {
      n <- Gen.choose(0, 1000)
      ft <- fingerTreeOfN[A](n)
    } yield ft


  implicit def arbFingerTree[A: Arbitrary]: Arbitrary[FingerTree[A]] =
    Arbitrary { fingerTree[A] }


  "Basic FingerTree constructors (examples)" - {

    // Experiment below with writing various trees (or in the REPL)

    "typecheck and compile test" in {

      Empty ()
      Single[Int] (1)
      Digit ('t')
      Digit ('t','h')
      Deep (Digit('t','h'), Empty(), Digit ('r','e','e'))
      Deep[Char] (Digit ('t','h'), Empty (), Digit ('r','e','e'))
      Deep (Digit (), Empty (), Digit ())

      // The following is the tree from page 4 in the paper. It seems to render
      // well without any type annotations

      Deep (
        Digit ('t','h'),
        Deep (
          Digit (Node2 ('i','s'), Node2('i','s')),
          Empty (),
          Digit (Node3 ('n','o','t'), Node2('a','t'))
        ),
        Digit ('r','e','e')
      )
    }
  }

  "Exercise 19 (extractors)" - {

    import FingerTree._

    "Nil on Empty" in {
      Empty () match {

         case Nil () =>
           Empty().empty should be (true)

         case _ =>
           fail ("Should not be happening!")
      }
    }

    "ConsL(_,Nil) on Single" in {

      Single (42) match {

        case ConsL(_, Nil ()) =>
          Single(42).nonEmpty should
            be (true)

        case _ =>
          fail ("This should not be happening!")
      }
    }

    val ft3plus = for {
      n <- Gen.choose(3,100)
      t <- fingerTreeOfN[Int] (n)
    } yield t

    "ConsL(_,Consl(_,_)) should match any tree larger than 3" in {

      forAll (ft3plus -> "t") {
        t: FingerTree[Int] =>
          whenever (t.size >= 3 ) { // work around shrinking
            t should
              matchPattern { case ConsL (a, ConsL (b,_)) => }
          }
      }
    }


    "Test the the left prefix of a folded addL tree (tests addL via toTree)" in {

      forAll ("l") {
        l: List[Int] =>
          whenever (l.size >=3 ) { // work around shrinking
            val t = FingerTree.toTree (l)
            t.headL should be (l.head)
            t.tailL.headL should be (l.tail.head)
            t.tailL.tailL.headL should be (l.tail.tail.head)
          }
      }
    }

    "ConsR(ConsR(_,_),_) should match any tree larger than 3" in {
      forAll (ft3plus -> "t") {
        t: FingerTree[Int] =>
          whenever (t.size >= 3 ) { // work around shrinking
            t should
              matchPattern { case ConsR (ConsR (_,_), _) => }
          }
      }
    }


    "List (0,0,0).toTree regressions (deepL)" in {
      val l = List (1,2,3)
      val t = FingerTree.toTree (l)
      t.tailL.headL should be (l.tail.head)
      t.headR should be (3)
      t.headL should be (1)
    }

    "Pattern should not be Nil on nonEmpty (12)" in {

      // This test wouldn't make sense in Haskell, because t.nonEmpty is decided
      // using patterns, but for us it is independent
      forAll { t: FingerTree[Int] =>
        whenever (t.nonEmpty) {
          t should not matchPattern { case Nil () => }
        }
      }
    }

    "ConsR (Nil,_) should match on Single (13)" in {
      Single(42) should
        matchPattern { case ConsR (Nil (),42) => }
    }

    "it should have the right suffix on any tree larger than 3 (15)" in {
      forAll ("l") { l: List[Int] =>
        whenever (l.size >= 3) {
          val t = FingerTree.toTree (l.reverse)
          withClue ("the last element") { t.headR should === (l.head) }
          withClue ("2nd last element") { t.tailR.headR should === (l.tail.head) }
        }
      }
    }

    "List(0, 1, 0, 0, 0, 0) regression" in {
      val l = List(2, 1, 0, 0, 0, 0)
      val t = FingerTree.toTree (l.reverse)
      withClue ("as 1st element") { l.head should ===  (t.headR) }
      withClue ("as 2nd element") { l.tail.head should === (t.tailR.headR) }
    }

  }

  // Exercise 17

  "Exercise 17 (Reduce[FingerTree])" - {
    "test" in{
      implicit val M = adpro.Monoid.intAddition
      forAll { fa: FingerTree[Int] =>
          val a = FingerTree.reduceTree.reduceR[Int,Int] (M.op _) (fa, M.zero)
          val b = FingerTree.reduceTree.reduceL[Int,Int] (M.op _) (M.zero , fa)
          a shouldBe b
        }
    }
    "Reduce[FingerTree] reducers collapse for monoids Int+" in {
      //hint:
      implicit val monoid = adpro.Monoid.intAddition
      FingerTree.reduceTree.Laws.reducersEquivalentForMonoids[Int]
    }

    "Reduce[FingerTree] reducers collapse for monoid Int*" in {
      implicit val monoid = adpro.Monoid.intMultiplication
      FingerTree.reduceTree.Laws.reducersEquivalentForMonoids[Int]
    }

    "Reduce[FingerTree] reducers collapse for string monoid" in {
      implicit val monoid = adpro.Monoid.stringMonoid
      FingerTree.reduceTree.Laws.reducersEquivalentForMonoids[String]
    }

    "Reduce[FingerTree] reducers collapse for list monoid" in {
      implicit val monoid = adpro.Monoid.listMonoid[Int]
      FingerTree.reduceTree.Laws.reducersEquivalentForMonoids[List[Int]]
    }
  }


  "Exercise 15 (toTree)" - {
    "toTree and toList should be inverses on List" in {
      forAll ("l") { l: List[Int] =>
        FingerTree.toTree (l).toList should
          be (l)
      }
    }

  }

  // Exercise 13 (tests to be written)

  "Exercise 13 (addR)" - {

    "addR produces a queue containing the inserted element (a)" in {
      FingerTree.addR(Empty(), 42).toList shouldBe List(42)
    }

    "addR produces a queue containing the inserted pair of elements (b)" in {
      FingerTree.addR(FingerTree.addR(Empty(), 42), 43).toList shouldBe List(42, 43)
    }

    "addR folded over a list of ints producrs a queue containing this list (c)" in {
      forAll ("l") { l: List[Int] =>
        val t1 = l.foldLeft[FingerTree[Int]](Empty())((acc,a)=>FingerTree.addR(acc,a))
        t1.toList shouldBe l
      }
    }
  }


  "Exercise 12 (addL)" - {

    "produce a queue containing the inserted element" in {
      Empty ().addL (42).toList should
        be (List (42))
    }

    "two consecutive addL get the order right (regression)" in {

      Empty ().addL (42).addL (41).toList should
        be (List (41,42))
    }

    "There is only one way to represent <41,42>, addL must comply (regression)" in {

      Empty ().addL (42).addL (41) should
        be (Deep (Digit (41), Empty (), Digit (42)))
    }

    "produce a queue containing the inserted elements" in {
      forAll (Gen.listOfN (100, Gen.choose[Int](0,1000)) -> "l") {
        l: List[Int] =>
          l.foldRight[FingerTree[Int]] (Empty ()) (FingerTree.addL)
           .toList should be (l)
      }
    }

  }

  "Exercise 11 (simple regressions to ensure minimal progress, the real tests are done in Ex 17)" - {

    val t = Deep (Digit (41), Empty (), Digit (42))
    val l = List (41, 42)

    "simple reducer regression (1)" in {
       t.toList should be (l)
     }

     "simple reducer regression (2)" in {
        reduce[FingerTree].
          reduceR[Int,List[Int]] (_::_) (t, List()) should
            be (l)
     }

  }


}
