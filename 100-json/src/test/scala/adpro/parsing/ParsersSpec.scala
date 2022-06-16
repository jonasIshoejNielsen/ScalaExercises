package adpro.parsing

// Many of these tests could still be moved to Parsers.Laws, to facilitate
// other implementations of parsers

import scala.language.implicitConversions

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

import scala.util.matching.Regex

import Exercises._

class ParsersSpec
    extends org.scalatest.freespec.AnyFreeSpec
    with org.scalatest.matchers.should.Matchers
    with org.scalatest.EitherValues
    with org.scalatest.Inside
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  import Exercises.{MyParsers => P}
  import P._
  import P.{regex => regex_ } // some conflict, likely with Scalatest

  implicit val arbLocation: Arbitrary[Location] =
    Arbitrary { for {
      s <- implicitly[Arbitrary[String]].arbitrary
      n <- Gen.choose (0, s.size)
    } yield Location (s,n) }


  val jsonTxt = """
  {
    "Company name" : "Microsoft Corporation",
    "Ticker"  : "MSFT",
    "Active"  : true,
    "Price"   : 30.66,
    "Shares outstanding" : 8.38e9,
    "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
  }
  """

  val malformedJson1 = """
  {
    "Company name" ; "Microsoft Corporation"
  }
  """

  val malformedJson2 = """
  [
    [ "HPQ", "IBM",
    "YHOO", "DELL" ++
    "GOOG"
    ]
  ]
  """

  val JP = new JSONParser (MyParsers)


  "Exercise 14 (non-terminals)" - {

    "json positive" in {
      P.run (JP.json) (jsonTxt) should
      be (Right (
        JObject(Map(
          "Shares outstanding" -> JNumber(8.38E9),
          "Price" -> JNumber(30.66),
          "Company name" -> JString("Microsoft Corporation"),
          "Related companies" -> JArray(
            Vector(JString("HPQ"), JString("IBM"),
              JString("YHOO"), JString("DELL"),
              JString("GOOG"))),
          "Ticker" -> JString("MSFT"),
          "Active" -> JBool(true))) ))
    }

    "json negative" in {
      P.run (JP.json) (malformedJson1) should
        matchPattern { case Left (_) => }
      P.run (JP.json) (malformedJson2) should
        matchPattern { case Left (_) => }
    }

    "jobject positive" in {
      P.run (JP.ws |* JP.jobject) (jsonTxt) should
      be (Right (
        JObject(Map(
          "Shares outstanding" -> JNumber(8.38E9),
          "Price" -> JNumber(30.66),
          "Company name" -> JString("Microsoft Corporation"),
          "Related companies" -> JArray(
            Vector(JString("HPQ"), JString("IBM"),
              JString("YHOO"), JString("DELL"),
              JString("GOOG"))),
          "Ticker" -> JString("MSFT"),
          "Active" -> JBool(true))) ))
    }

    "jobject negative" in {
      P.run (JP.jobject) (malformedJson1) should
        matchPattern { case Left (_) => }
      P.run (JP.jobject) (malformedJson2) should
        matchPattern { case Left (_) => }
    }


    "field positive" in {
      val s = """"price":30.66"""
      P.run (JP.field) (s) should
        be (Right ("price" -> JNumber (30.66)))
    }

    "field negative" in {
      val s = """"hpq" :"""
      P.run (JP.field) (s) should
        matchPattern { case Left (_) => }
    }


    "jarray positive" in {
      val s = """[ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]"""
      P.run (JP.jarray) (s) should
        be (Right (
          JArray (Vector (
            JString ("HPQ"),
            JString ("IBM"),
            JString ("YHOO"),
            JString ("DELL"),
            JString ("GOOG")
          ))
      ))
    }

    "jarray negative" in {
      val s = """[ "DELL" ++ "GOOG" ]"""
      JP.jarray (Location (s, 0)) should
        matchPattern { case Failure (_) => }
    }

    "jarray empty input" in {
      JP.jarray (Location ("", 0)) should
        matchPattern { case Failure (_) => }
    }

  }

  "Exercise 13 (terminals)" - {

    "jnumber positive" in {
      val s = "-0.42e+42"
      P.run (JP.jnumber) (s) should
        be (Right (JNumber (-0.42e+42)))
    }

    "jnumber negative" in {
      val s = """Company name"""
      P.run (JP.jnumber) (s) should
        matchPattern { case Left (_) => }
    }


    "jstring positive" in {
      val s = """"Company name""""
      P.run (JP.jstring) (s) should
        be (Right (JString ("Company name")))
    }

    "jstring negative" in {
      val s = """Company name"""
      P.run (JP.jstring) (s) should
        matchPattern { case Left (_) => }
    }

    "jbool positive" in {
      P.run (JP.jbool) ("true") should
        be (Right (JBool (true)))
      P.run (JP.jbool) ("false") should
        be (Right (JBool (false)))
    }

    "jbool negative" in {
      P.run (JP.jbool) (" null") should
        matchPattern { case Left (_) => }
      P.run (JP.jbool) ("42") should
        matchPattern { case Left (_) => }
    }

    "jnull positive" in {
      P.run (JP.jnull) ("null") should
        be (Right (JNull))
    }

    "jnull negative" in {
      P.run (JP.jnull) (" null") should
        matchPattern { case Left (_) => }
      P.run (JP.jnull) ("42") should
        matchPattern { case Left (_) => }
    }

  }

  "Exercise 12 (tokens)" - {

    "ws positive" in {
      forAll { (n1: Int, s1: String) =>
        val n = Math.abs (n1 % 100) + 1
        val s = "\t\n " * n + s1
        val l = Location (s, 0)
        inside (JP.ws (l)) {
          case Success ((), m) =>
            m should be >= (n * 3)
          case _ => fail ("Should not happen!")
        }
      }
    }

    "ws negative" in {
      val s = "e12343 \t\n"
      P.run (JP.ws) (s) should
        matchPattern { case Left (_) => }
    }

    "DOUBLE positive" in {
      forAll { x: Double =>
        val s = Math.abs(x).toString.toLowerCase
        P.run (JP.DOUBLE) (s) should
          be (Right (s.toDouble))
        P.run (JP.DOUBLE) ("+" + s) should
          be (Right (s.toDouble))
        P.run (JP.DOUBLE) ("-" + s) should
          be (Right (-s.toDouble))
      }
    }

    "DOUBLE negative" in {
      val s = "e12343"
      P.run (JP.DOUBLE) (s) should
        matchPattern { case Left (_) => }
    }

    "QUOTED positive" in {
      val s = """"Company name""""
      P.run (JP.QUOTED) (s) should
        be (Right ("Company name"))
    }

    "QUOTED negative" in {
      val s = """Company name"""
      P.run (JP.QUOTED) (s) should
        matchPattern { case Left (_) => }
    }
  }

  "Laws from the book also for non-exercise code (all shall pass when done)" - {

    "runChar (book)" in (Laws.runChar)

    "char consumes one char" in {
      val p = P.char ('a')
      val l = Location ("aaa", 0)

      withClue ("offset 0:") {
        p (l) should be (Success ('a', 1))}

      withClue ("offset 1:") {
        p (l advanceBy 1) should be (Success ('a', 1)) }

      withClue ("offset 2:") {
        p (l advanceBy 2) should be (Success ('a', 1)) }
    }

    "*|" in {
      val p  = "abra" *| "cadabra"
      p (Location ("xabracadabra", 1)) should
        be (Success ("abra", 11))
    }

    "|*" in {
      val p  = "abra" |* "cadabra"
      p (Location ("xabracadabra", 1)) should
        be (Success ("cadabra", 11))
    }

    "?" in {
      val p  = "abra" |* "cadabra".?
      p (Location ("xabra", 1)) should
        be (Success (None, 4))
      p (Location ("xabracadabra", 1)) should
        be (Success (Some ("cadabra"), 11))
    }

    "*" in {
      val p  = P.string ("abra").*
      p (Location ("xcadabra",1)) should
        be (Success (Nil, 0))
      p (Location ("xabra",1)) should
        be (Success (List ("abra"), 4))
      p (Location ("xabraabracadabra", 1)) should
        be (Success (List ("abra", "abra"), 8))
    }

  }

  "Exercise 11 (regex)" - {

    "regex positive" in {

      val r = "(a|b)+".r

      implicit val arbInt: Arbitrary[Int] =
        Arbitrary (Gen.choose (1,100))

      forAll ("n","m") { (n: Int, m: Int) =>
        val s = "b" * n + "a" * m + "x"
        val l = Location (s, 0)

        r (l) should
          be (Success (s dropRight 1, s.size - 1))
      }
    }


    "regex negative" in {

      val r = """(a|x)+""".r

      forAll { l: Location =>
        whenever (r.findPrefixOf (l.input) == None) {
          r (l) should
            matchPattern { case Failure (_) => }
        }
      }
    }

  }

  "Exercise 10 (listOfN)" - {

    "listOfN 1" in
      Laws.listOfN1

    "listOfN 2" in
      Laws.listOfN2

    "listOfN 3" in
      Laws.listOfN3

    "listOfN 4" in
      Laws.listOfN4

    "listOfN 5" in
      Laws.listOfN5

    "listOfN fail" in
      Laws.listOfNFail

  }


  "Exercise 9 (many1)" - {

    "many1 should type check" in {
      "def test: Parser[List[Char]] = P.many1 ('1')" should
        compile
    }

    "many1 success" in
      Laws.many1Success

    "many1 fail" in
      Laws.many1Fail

    "many1 'ax' regressions" in {

      P.run ("a") ("ax") should
        be (Right ("a"))

      P.run (many1 ("a")) ("xa") should
        matchPattern { case Left (_) => }

      P.run (many1 ("a")) ("axa") should
        be (Right (List ("a")))

      P.run (many1 ("a")) ("aaxa") should
        be (Right (List ("a", "a")))

      P.run (many1 ("a")) ("aaaxa") should
        be (Right (List ("a", "a", "a")))
    }

  }


  // This gets also tested with listOfN
  "Exercise 8 (product)" - {

    "product success" in {
      ("aa" ** "bb") (Location ("xaabb",1)) should
        be (Success ("aa" -> "bb", 4)) }

    "product right fail" in {
      ("aa" ** "bb") (Location ("xaacc",1)) should
        matchPattern { case Failure (_) =>  } }

    "product left fail" in {
      ("aa" ** "bb") (Location ("xccbb",1)) should
        matchPattern { case Failure (_) =>  } }

  }


  // This test gives out the solution
  "Exercise 7 (manyA)" - {

    "manyA should type check" in {
      "def a: Parser[Int] = MyParsers.manyA" should
        compile
    }

    "manyA positive test" in {
      forAll (Gen.choose (0, 1000)){ m: Int =>
        val n = Math.abs (m)
        val s = "a" * n + "x"
        P.run (P.manyA) (s) should
          be (Right (n))
      }
    }

    "manyA regression 'aaax'" in {
      P.run (P.manyA) ("aaax") should
        be (Right (3))
    }

    "manyA regression 'aax'" in {
      P.run (P.manyA) ("aax") should
        be (Right (2))
    }

    "manyA regression 'ax'" in {
      P.run (P.manyA) ("ax") should
        be (Right (1))
    }

    "manyA regression 'x'" in {
      P.run (P.manyA) ("x") should
        be (Right (0))
    }
  }


  "Exercise 6 (many)" - {

    "many can multiply any string parser" in
      Laws.manyString

    "many regression 'aaax'" in {
      P.run (P.many (P.char ('a'))) ("aaax") should
        be (Right (List ('a', 'a', 'a')))
    }

  }

  "Exercise 5 (or)" - {

    "or left" in
      Laws.orLeft

    "or right" in
      Laws.orRight

    "or fail" in
      Laws.orFail
  }

  "Exercise 4 (string)" - {

    "string positive (Laws.runString)" in
      Laws.runString

    "string empty" in
      Laws.stringEmpty

    "string negative" in
      Laws.stringNegative

    "string one" in {
      val l = Location ("aaa", 0)
      withClue ("offset 0:") {
        P.string ("a") (l) should be (Success ("a", 1))}
    }
  }

  "Exercise 3 (map2)" - {

    "map2 with two successes" in Laws.map2withTwoSuccesses

    // Slighly annoying with the current design that failure tests cannot
    // be written abstractly because we have not abstracted failures,
    // and have no way to create a default error message.  This is, of
    // course, possible to do, but I prefer not to depart from the book
    // so far.

    "map2 with left failure" in {
      val p = P.map2[Int,Int,Int] (P.succeed (42), P.fail) (_+_)
      forAll { input: String =>
        P.run (p) (input) should matchPattern { case Left (_) => }
      }
    }

    "map2 with right failure" in {
      val p = P.map2[Int,Int,Int] (P.fail, P.succeed (42)) (_+_)
      forAll { input: String =>
        P.run (p) (input) should matchPattern { case Left (_) => }
      }
    }

    "map2 regression 'aaax'" in {
      val p = P.string ("a")
      val p2 = P.map2 (p, p) { (a,b) => List (a, b) }
      val p3 = P.map2 (p, p2) (_::_)
      P.run (p3) ("aaax") should
        be (Right (List ("a", "a", "a")))
    }

    "map2 regression 'aaax' with succeed" in {
      val p = P.string ("a")
      val p2 = P.map2 (p, p) { (a,b) => List (a, b) }
      val p3 = P.map2 (P.succeed ("a"), p2) (_::_)
      P.run (p3) ("aaax") should
        be (Right (List ("a", "a", "a")))
    }

    "map2 regression 'aax'" in {
      val p = P.char ('a')
      val p2 = P.map2 (p, p) { (a,b) => List (a, b) }
      P.run (p2) ("aaax") should
        be (Right (List ('a', 'a')))
    }


  }

  "Exercise 2 (map)" - {

    "Simplified structure preservation" in
      Laws.mapStructurePreserving2succeed

    // This is really a test for flatMap, but since we do not really
    // have an exercise on flatMap (it is pre-solved), it can stay here.
    "map keeps the number of consumed chars (regression)" in {
      forAll { l: Location =>
        val k = Math.min (5, l.cursor.size)
        val s = l.cursor.substring (0, k)
        val p = P.string (s)
        val pm = p map { _ => 42 }
        inside (p (l), pm (l)) {
          case (Success (t, n), Success (42, m)) =>
            n should be (m)
            t should be (s)
          case (Failure (_), Failure (_)) =>
          case _ => fail ("Should not happen!")
        }
      }
    }

    "map fail" in {
      val err = ParseError (Nil)
      val p: Parser[Int] = P.fail (err) map[Int] ( (n: Nothing) => 42 )
      forAll { l: Location =>
        p (l) should be (Failure (err)) }
    }

  }

  "Exercise 1 (succeed)" - {
    "succeed as defined in the book" in
      Laws.succeed

    "succeed observes location offset" in {
      forAll { (l: Location, n: Int) =>
        inside (P.succeed (n) (l)) {
          case Success (get, charsConsumed) =>
            get should be (n)
            charsConsumed should be (0)
          case _ => fail ()
        }
      }
    }

  }

}
