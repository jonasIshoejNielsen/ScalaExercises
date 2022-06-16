// Andrzej Wąsowski, IT University of Copenhagen, Advanced Programming
package adpro.parsing

import java.util.regex._
import scala.util.matching.Regex

// Need this for higher kinded polymorphism
import language.higherKinds
// Need this for introducing internal DSL syntax
import language.implicitConversions

trait Parsers[ParseError, Parser[+_]] { self =>

  def run[A] (p: Parser[A]) (input: String): Either[ParseError,A]
  implicit def char (c: Char): Parser[Char]
  implicit def string(s: String): Parser[String]
  implicit def operators[A] (p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  def or[A] (s1: Parser[A], s2: =>Parser[A]): Parser[A]
  def listOfN[A] (n: Int, p: Parser[A]): Parser[List[A]]

  def map[A,B] (p: Parser[A]) (f: A => B): Parser[B]
  def many[A](p: Parser[A]): Parser[List[A]]
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)
  def slice[A](p: Parser[A]): Parser[String]
  def product[A,B](p: Parser[A], p2: =>Parser[B]): Parser[(A,B)]
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]
  implicit def asStringParser[A] (a: A) (implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {

    def |[B>:A] (p2: Parser[B]): Parser[B] = self.or (p,p2)
    def or[B>:A] (p2: Parser[B]): Parser[B] = self.or (p,p2)
    def **[B] (p2: Parser[B]): Parser[(A,B)] = self.product (p,p2)
    def product[B] (p2: Parser[B]): Parser[(A,B)] = self.product (p,p2)

    def map[B] (f: A => B): Parser[B] = self.map (p) (f)
    def flatMap[B] (f: A => Parser[B]): Parser[B] = self.flatMap (p) (f)
    def many: Parser[List[A]] = self.many[A] (p)
    def slice: Parser[String] = self.slice (p)
  }

  object Laws {

    // Storing the laws in the trait -- the will be instantiated when we have
    // concrete implementation.  Still without a concrete implementation they
    // can be type checked, when we compile.  This tells us that the
    // construction of the laws is type-correct (the first step for them
    // passing).

    import org.scalacheck._
    import org.scalacheck.Prop._

    val runChar = Prop.forAll { (c: Char) => run(char(c))(c.toString) == Right(c) }
    val runString = Prop.forAll { (s: String) => run(string(s))(s) == Right(s) }

    val listOfN1 = Prop.protect (run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad"))
    val listOfN2 = Prop.protect (run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab"))
    val listOfN3 = Prop.protect (run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab"))
    val listOfN4 = Prop.protect (run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad"))
    val listOfN5 = Prop.protect (run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab"))
    val listOfN6 = Prop.protect (run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab"))

    def succeed[A] (a: A) = Prop.forAll { (s: String) => run(self.succeed(a))(s) == Right(a) }

    // Not planning to run this (would need equality on parsers), but can write
    // for typechecking:

    def mapStructurePreserving[A] (p: Parser[A]): Boolean =
      map(p)(a => a) == p
  }


  // Exercise 1 (Any is used as a placeholder, and should be removed)
    //note return how many is parsed i.e. Parser[Int]
  def manyA: Parser[Int] = many(char('a')).map(lst=>lst.size)

  // Exercise 2 (The entire type signature is wrong (just a temporary to make
  // the file compile). Replace it with the right one)
  //NOTE: product=zip
  def map2[A,B,C] (p: Parser[A], p2: Parser[B]) (f: (A,B) => C): Parser[C] =
    product(p, p2).map(v=>f(v._1, v._2))

  //NOTE: do p once then many p
  def many1[A] (p: Parser[A]): Parser[List[A]] = 
    map2(p, many(p))(_::_) 

  // Exercise 3 (Any is used as a placeholder, and should be removed)

  def digitTimesA: Parser[Int] =
    map("[0-9]".r.regex)(_.toInt)
    .flatMap(i=>listOfN(i, char('a'))
    .map(_.size) )

  // Exercise 4 (fix the type signature - the current is just a placeholder)

  def map2_ [A,B,C] (p: Parser[A], p2: Parser[B]) (f: (A,B) => C): Parser[C] =
    flatMap(p) {a => map(p2) {b => f(a,b) } }

  def product_[A,B] (p: Parser[A], p2: =>Parser[B]): Parser[(A,B)] =
    flatMap(p) {a => map(p2) {b => (a,b) } }

  // Exercise 5 (fix the type signature - the current is just a placeholder)

  def map_ [A,B] (p: Parser[A]) (f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))

}
