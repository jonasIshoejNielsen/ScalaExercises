package adpro.parsing

import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

class ParsersSpec
    extends org.scalatest.freespec.AnyFreeSpec
    with org.scalatest.matchers.should.Matchers
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

    // The pattern for tests is a bit complex for type errors.  We first
    // introduce a dependency on the solution, so that the test code is
    // recompiled every time the main code is changed.
    trait ParsersDependency[PE, P[+_]] extends Parsers [PE, P] {
      def dependency = List(
        this.map_[Any,Any] _,
        this.map2_[Any,Any,Any] _,
        this.product_ [Any,Any] _,
        this.digitTimesA,
        this.map2[Int,Int,Int] _,
        this.many1[Unit] _,
        this.manyA
      )
    }
    // Without the above the code below may reflect the result of compilation
    // with an old version of the main module, so the test result is misleading.
    // (The build system does not recognize that the test should be recompiled,
    // because the dependency is hidden in a String value).

  "Exercise 5 (map_)" - {

    "map_ should type check in context" in {
      """trait TestParsers[PE, P[+_]] extends Parsers [PE, P] {
        |   def test =
        |      map_[Int,Unit] (this.manyA) { (x: Int) => () }
        |}
      """.stripMargin should compile
    }
  }

  "Exercise 4 (map2_, product_)" - {

    "map2_ should type check in context" in {
      """trait TestParsers[PE, P[+_]] extends Parsers [PE, P] {
        |   def test =
        |      map2_[Int,String,Unit] (this.manyA, this.string ("test")) {
        |         (x: Int, y: String) => () }
        |}
      """.stripMargin should compile
    }

    "product_ should type check in context" in {
      """trait TestParsers[PE, P[+_]] extends Parsers [PE, P] {
        |   def test =
        |      product_[Int,String] (this.manyA, this.string ("test"))
        |         .map[Unit] { (xy: (Int, String)) => () }
        |}
      """.stripMargin should compile
    }
  }

  "Exercise 3 (digitTimesA)" - {

    "digitTimesA should type check in context" in {
      """trait TestParsers[PE, P[+_]] extends Parsers [PE, P] {
        |   def test =
        |     digitTimesA.map[Unit] { (n: Int) => ( )}
        |
        |}
      """.stripMargin should compile
    }
  }


  "Exercise 2 (map2, many1)" - {

    "map2 should compile" in {
      """trait TestParsers[PE, P[+_]] extends Parsers [PE, P] {
        |   def test =
        |      map2[Int,String,Unit] (this.manyA, this.string ("test")) {
        |         (x: Int, y: String) => () }
        |}
      """.stripMargin should compile
    }
    "many1 should type check in context" in {

      """trait TestParsers[PE, P[+_]] extends Parsers [PE, P] {
        |   def test: P[Unit] =
        |      many1[String] (this.string ("test"))
        |         .map { (l: List[String]) => () }
        |}
      """.stripMargin should compile
    }
  }

  "Exercise 1 (manyA)" - {

    "manyA should type check in context" in {
      """trait TestParsers[PE, P[+_]] extends Parsers [PE, P] {
        |   def test = this.map[Int,Int] (this.manyA) { (n: Int) => n + 1 }
        |}
      """.stripMargin should compile
    }

  }

}
