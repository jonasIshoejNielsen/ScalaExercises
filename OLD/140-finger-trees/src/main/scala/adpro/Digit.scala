package adpro

import Reduce.reduce

// Several convenience operations for Digits.
object Digit {

  type Digit[A] = List[A]

  // A factory method that allows us to use Digit (...)
  // like a constructor

  def apply[A] (as: A*): Digit[A] =
    List (as:_*)

  // This is an example of extractor, so that we can use Digit(...) in pattern
  // matching.  Case classes have extractors automatically, but Digit defined
  // as above is not a case class, but just a type name.

  def unapplySeq[A] (d: Digit[A]): Option[Seq[A]] =
    Some (d)
}


