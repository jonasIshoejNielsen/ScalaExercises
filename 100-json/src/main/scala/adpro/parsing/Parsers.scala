// Andrzej Wąsowski, IT University of Copenhagen, Advanced Programming
// based on fpinscala exercises
package adpro.parsing

import language.higherKinds
import language.implicitConversions

import scala.util.matching.Regex

object Exercises {

  trait Parsers[ParseError,Parser[+_]] { self =>

    def flatMap[A,B] (p: Parser[A]) (f: A => Parser[B]): Parser[B]

    def or[A] (s1: Parser[A], s2: => Parser[A]): Parser[A]

    // Exercise 1 starts in MyParsers below

    // Exercise 2

    def map[A,B] (p: Parser[A]) (f: A => B): Parser[B] =
      p.flatMap(a => succeed(f(a)))

    // Exercise 3

    def map2[A,B,C] (p1: Parser[A], p2: => Parser[B]) (f: (A,B) => C) : Parser[C] = 
      p1.flatMap(a1=>p2.map(a2=>f(a1,a2)))

    // Exercise 4 continues in MyParsers below

    // Exercise 6
    //NOTE: create parser with 1..* p or empty list if no p
    // Will cause stack overflow if p is not consuming any chars
    def many[A] (p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _) or succeed(List())

    // Exercise 7
    //match 'a' and return how size of those found
    def manyA: Parser[Int] =
      many(string("a")).map(lst => lst.size)

    // Exercise 8

    def product[A,B] (p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
      p1.flatMap(a1=>p2.map(a2=>(a1,a2)))

    // Exercise 9
    //same as exercise 7 but with no or succeed
    def many1[A] (p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _) 

    // Exercise 10
      //NOTE: check if n<=0 and then recursive using map2
    def listOfN[A] (n: Int, p: Parser[A]): Parser[List[A]] = (n<=0) match{
      case true   => succeed(Nil)
      case false  => map2(p, listOfN(n-1, p)) (_::_)
    }

    // Exercise 11 continues in MyParsers below

    def slice[A] (p: Parser[A]): Parser[String]
    def succeed[A] (a: A): Parser[A]
    def fail (err: ParseError): Parser[Nothing]

    def run[A] (p: Parser[A]) (input: String): Either[ParseError,A]

    implicit def char (c: Char): Parser[Char] =
      string (c.toString) map { _ => c }

    implicit def operators[A] (p: Parser[A]): ParserOps[A] =
      ParserOps[A] (p)

    implicit def regex (r: Regex): Parser[String]

    implicit def string (s: String): Parser[String]

    implicit def asStringParser[A] (a: A)
      (implicit f: A => Parser[String]): ParserOps[String] =
        ParserOps (f (a))

    case class ParserOps[A] (p: Parser[A]) {

      def |[B>:A] (p2: => Parser[B]): Parser[B] =
        self.or (p :Parser[A],p2: Parser[B]) : Parser[B]

      def |*[B] (p2: => Parser[B]): Parser[B] =
        self.slice (p).flatMap { _ => p2 }

      def *| (p2: => Parser[Any]): Parser[A] =
        (p ** p2) map { case (a,_) => a }

      def ? : Parser[Option[A]] =
        ( p map { a => Some (a) } ) | succeed (None)

      def * : Parser[List[A]] =
        self.many (p)

      def or[B >: A] (p2: => Parser[B]): Parser[B] =
        self.or (p, p2)

      def **[B] (p2: => Parser[B]): Parser[(A,B)] =
        self.product (p, p2)

      def product[B] (p2: => Parser[B]): Parser[(A,B)] =
        self.product (p, p2)

      def map[B] (f: A => B): Parser[B] =
        self.map (p) (f)

      def flatMap[B] (f: A => Parser[B]): Parser[B] =
        self.flatMap (p) (f)

      def many: Parser[List[A]] =
        self.many[A] (p)

      def slice: Parser[String] =
        self.slice (p)
    }


    object Laws {

      // Storing the laws in the trait -- the will be instantiated when we
      // have concrete implementation.  Still without a concrete
      // implementation they can be type checked, when we compile.  This tells
      // us that the construction of the laws is type-correct (the first step
      // for them passing).  I reformulated the laws using the (somewhat
      // impure) API of scalatest, in the interest of using a single popular
      // library.  I also added more laws.

      import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._
      import org.scalatest.matchers.should.Matchers._

      def runChar =
        forAll { c: Char =>
          run (char(c)) (c.toString) should
            be (Right (c)) }

      def listOfN1 =
        run (listOfN (3, "ab" | "cad")) ("ababcad") should
          be (Right ("ab" :: "ab" :: "cad" :: Nil))

      def listOfN2 =
        run (listOfN(3, "ab" | "cad")) ("cadabab") should
          be (Right("cad" :: "ab" :: "ab" :: Nil))

      def listOfN3 =
        run (listOfN(3, "ab" | "cad")) ("ababab") should
          be (Right("ab" :: "ab" :: "ab" :: Nil))

      def listOfN4 =
        run (listOfN(2, "ab" | "cad")) ("cadabab") should
          be (Right("cad" :: "ab" :: Nil))

      def listOfN5 =
        run (listOfN(1, "ab" | "cad")) ("ababab") should
          be (Right("ab" :: Nil))

      def listOfNFail =
        run (listOfN (3, "ab" | "cad")) ("ababxcad") should
          matchPattern { case Left (_) => }

      def succeed =
        forAll { (a: Int, s: String) =>
          self.run (self.succeed (a)) (s) should
            be (Right(a)) }

      // Not planning to run this (would need equality on parsers),
      // but can write for typechecking:

      def mapStructurePreserving[A] (p: Parser[A]) =
         map (p) (identity) should
           be (p)

      // but this test is possible to run once we implement things

      def mapStructurePreserving2succeed =
        forAll { (s: String, n: Int) =>
          val p = self.succeed (n)
          val p1 = self.map (p) (a => a)
          self.run (p) (s) should
            be (self.run (p1) (s))
        }

      def map2withTwoSuccesses = {
        val p = self.map2 (self.succeed (42), self.succeed (-42)) (_+_)
        forAll { input: String =>
          self.run (p) (input) should
            be (Right (0)) }
      }


      def runString =
        forAll { s: String =>
          self.run (self.string (s)) (s) should be (Right (s)) }


      def stringEmpty =
        forAll { s: String =>
          self.run (self.string ("")) (s) should be (Right ("")) }

      def stringNegative =
        forAll ("s", "t") { (s: String, t: String) =>
          whenever (!t.startsWith (s)) {
            self.run (string (s)) (t) should
              matchPattern { case Left (_) => }
          }
        }


      def orLeft =
        self.run (self.or (string ("abra"), string ("cadabra"))) ("abra ") should
          be (Right ("abra"))

      def orRight =
        self.run (self.or( string("abra"), string("cadabra"))) ("cadabra") should
         be (Right ("cadabra"))

      def orFail =
        self.run (self.or( string("abra"), string("cadabra"))) ("blablab") should
          matchPattern { case Left (_) => }



      def manyString = {

        forAll ("s", "n") { (s: String, n: Int) =>
          whenever (s.size > 0 && !s.contains ('x')) {

            val k = n % 100
            val t = s.substring (0, Math.min (10, s.size))
            val p = string (t)
            val input = t * k + "xx" + t
            val output = List.fill (k) (t)

            self.run (many (p)) (input) should
              be (Right (output))
          }
        }
      }


      def many1Success = {

        forAll ("s", "n") { (s: String, n: Int) =>
          whenever (s.size > 0 && !s.contains ('x')) {

            val k = Math.abs (n % 100) + 1
            val t = s.substring (0, Math.min (10, s.size))
            val input = t * k + "x" + t
            val output = List.fill (k) (t)

            self.run (many1 (t)) (input) should
              be (Right (output))
          }
        }

      }


      def many1Fail = {
        forAll ("s", "t") { (s: String, t: String) =>

          val q = t.substring (0, Math.min (10, t.size))

          whenever (! s.startsWith (q)) {

            self.run (many1 (t)) (s) should
              matchPattern { case Left (_) => }
          }
        }
      }

    } // Laws

  } // Parsers


  // Concrete implementation of our parsers

  case class ParseError (stack: List[(Location, String)])

  case class Location (input: String, offset: Int = 0) {

    def cursor: String =
       input.substring (offset)

    def advanceBy (numChars: Int): Location =
       Location (input, offset + numChars)

    def toError (msg: String): ParseError =
       ParseError (List ((this, msg)))

  }

  type Parser[+A] = Location => Result[A]

  trait Result[+A] {

    // Adjust the number of characters consumed while producing the result.
    // So far only matters for Success.
    def advanceCharsConsumed (n: Int): Result[A] =
      this match {
        case Success (get, charsConsumed) =>
          Success (get, n + charsConsumed)
        case Failure (_) => this
      }

  }

  // CharsConsumed is the number of chars consumed by this particular
  // parser that produced the result (the total number of consumed characters
  // could be higher if other parsers have already consumed some part of the
  // input)

  case class Success[+A] (get: A, charsConsumed: Int)
     extends Result[A]

  case class Failure (get: ParseError)
     extends Result[Nothing]

  object MyParsers
     extends Parsers[ParseError, Parser] {

    def run[A] (p: Parser[A]) (input: String): Either[ParseError,A] =
      p (Location (input, 0)) match {

        case Success (a, n) => Right (a)
        case Failure (err) => Left (err)
      }

    def slice[A] (p: Parser[A]): Parser[String] =
      l => p (l) match {
        case Success (_, n) => Success (l.cursor.substring (0,n), n)
        case f @ Failure (_) => f
      }

    def fail (err: ParseError): Parser[Nothing] =
      loc => Failure (err)

    // for convenience of writing tests
    def fail: Parser[Nothing] = fail (err = ParseError (Nil))

    def flatMap[A,B] (p: Parser[A]) (f: A => Parser[B]): Parser[B] =
      loc => p (loc) match {
        case Success (a, n) =>
          f (a) (loc advanceBy n) advanceCharsConsumed (n)
        case failure @Failure (_) => failure
      }

    // Exercise 1
    //NOTE: Parser[A] = Location => Result[A]

    def succeed[A] (a: A): Parser[A] =
      loc => Success (a, 0)

    // Exercise 2 continues in the Parsers trait above

    // Exercise 4
    //loc.cursor.startsWith(s)
    //NOTE: Failure(ParseError) ParseError (stack: List[(Location, String)])
      //if find s--> report s and size of s
      //else: failure loc, s
    implicit def string (s: String): Parser[String] = loc => loc.cursor.startsWith(s) match {
      case true => Success (s, s.size)
      case false => Failure(ParseError(List(( loc,s))))
    }

    // Exercise 5
    //run p1 and if successfull return that else run p2
    def or[A] (p1: Parser[A], p2: => Parser[A]): Parser[A] = loc => p1(loc) match{
      case Success(a,n) => Success(a,n)
      case _ => p2(loc)
    }

    // Exercise 6 continues in the abstract trait Parsers

    // Exercise 11
      //NOTE: r.findFirstIn(loc.cursor)
    implicit def regex (r: Regex): Parser[String] = loc => r.findFirstIn(loc.cursor) match {
      case None => Failure(ParseError(List((loc,loc.cursor))))
      case Some(s) => string(s)(loc)   //Success (s, s.size)
    }


    // Exercise 12 continues below in the JSON parser

  }

  // An example implementation of JSON parser (it uses the abstract parser
  // interface, but it is tested with the concrete implementation above)

  trait JSON

  case object JNull extends JSON
  case class JNumber (get: Double) extends JSON
  case class JString (get: String) extends JSON
  case class JBool (get: Boolean) extends JSON
  case class JArray (get: IndexedSeq[JSON]) extends JSON
  case class JObject (get: Map[String, JSON]) extends JSON

  class JSONParser[ParseError, Parser[+_]] (P: Parsers[ParseError,Parser]) {

    import P._

    // Exercise 12
      //match \" and take what is inbetween
    lazy val QUOTED: Parser[String] =
      string("\"") |* regex("""([^"]*)""".r) *| string("\"")

    lazy val DOUBLE: Parser[Double] =
      """(\+|-)?[0-9]+(\.[0-9]+((e|E)(-|\+)?[0-9]+)?)?""".r.map(_.toDouble)
      //\s+
    lazy val ws: Parser[Unit] =
      """\s+""".r |* succeed(())

    // Exercise 13

    lazy val jnull: Parser[JSON] =
      "null".r |* succeed(JNull)

    lazy val jbool: Parser[JBool] =
      or("true".r, "false".r).map(s => JBool(s=="true"))

    lazy val jstring: Parser[JString] =
      QUOTED.map(JString(_)) 

    lazy val jnumber: Parser[JNumber] =
      DOUBLE.map(JNumber(_)) 

    // Exercise 14
      //[entries]   entries = multiple json with potential spaces and ,
    lazy val jarray: Parser[JArray] ={
      val start = string("[") ** ws.?
      val entries = many(json *| ws.? *| string(",").? *| ws.?)
      val end = string("]")
      (start |* entries *| end).map(lst => JArray(lst.toIndexedSeq))
    }

    lazy val field: Parser[(String,JSON)] =
      map2(QUOTED *| (ws.? ** string(":") ** ws.?), json) ((s,js) => (s,js))


    lazy val jobject: Parser[JObject] = {
      val start = string("{") ** ws.?
      val fields = many(field *| ws.? *| string(",").? *| ws.?)
      val end = string("}")
      (start |* fields *| end).map(lst => JObject(lst.toMap))
    }

    lazy val json: Parser[JSON] =
      ws.? |* { jstring | jnumber | jnull |  jbool | jarray | jobject }
      //ws.? |* or(jarray, or(jobject, or(jnull, or(jbool, or(jstring, jnumber)))))


  } // JSONParser

} // Exercises
