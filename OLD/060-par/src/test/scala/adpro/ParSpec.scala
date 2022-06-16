// wasowski, Advanced Programming, IT University of Copenhagen
// change package to adpro.SOLUTIONS to test teacher's solutions
package adpro

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import java.util.concurrent._

class ParSpec 
    extends org.scalatest.freespec.AnyFreeSpec 
    with org.scalatest.matchers.should.Matchers
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  import Par._

  def listOfNInts (n: Int) (implicit arb: Arbitrary[Int]): Gen[List[Int]] =
    Gen.listOfN[Int] (n, arb.arbitrary)

  // Increase here if tests are failing for no apparent reasons 
  // Testing concurrency in a rock solid hardware independent manner, not easy ...
  val TIMEOUT: Int = 50 // miliseconds
  
  val pool1 = Executors.newFixedThreadPool (1)
  val pool2 = Executors.newFixedThreadPool (2)
  val pool5 = Executors.newFixedThreadPool (5)
  val pool16 = Executors.newFixedThreadPool (16)

  object gadget  { // purposely imperative, we create data races to test!

    def reset = {
      counter = 0
      this
    }

    var counter: Int = 0;

    val task: Int => Int = 
      n => {
        val result = counter + 1
        Thread.sleep (TIMEOUT)
        counter = result
        n
      }

    val parCrash: Par[Any] = lazyUnit (???)
    val parInt: Par[Int] = lazyUnit (task (42))
    val listPar: List[Par[Int]] = List (1,2,3,4).map (unit[Int] _)

    def assertEqual[A] (p: Par[A], n: Int): Par[Unit] = 
      map2 (p, lazyUnit (this.counter should be (n))) { (_,_) => () }

    def assertLT[A] (p: Par[A], n: Int): Par[A] = 
      map2 (p, lazyUnit (this.counter should be < (n))) { (a,_) => a }
  }


  "Exercise 11" - {

    "This is not a test, but an easy way to run your function (prints first line from up to 100 chars from 3 websites)" in {
      for {
        html <- Par.wget ("https://www.itu.dk", "https://www.google.com", "https://www.twitter.com")
        head = html.split ("\n").head.take (100)
      } yield info (head)

    }
  }


  "Exercise 10" - {

    "An extension for map is created (we only test types)" in {
      """
         import Par._
         def f (l: Par[Int]): Par[String] = l.map (_.toString)
      """ should compile
    }

    "An extension for map2 is created (we only test types)" in {
      """
        import Par._
        def f (l: Par[Int], r: Par[Int]): Par[String] = 
          l.map2 (r) ((l,r) => l.toString + r.toString)
      """ should compile
    }

    "An extension for chooser is created (we only test types)" in {
      """
        import Par._
        def f[A] (pa: Par[A]) (pm: A => Par[Int]): Par[Int] = 
          pa.chooser[Int] (pm) 
      """ should compile
    }

  }


  "Exercise 9" - {

    "join really joins (the test is a bit weak)" in {
      forAll ("n") { n: Int =>
        val test: Par[Int] = join (lazyUnit (lazyUnit (42)))
        val result = Par.run (pool5) (test)
        result should be (42)
      }
    }
  }

  "Exercise 8" - {

    "chooser returns the right computation" in {
      forAll (Gen.choose (0,100) -> "n") { n: Int =>
         forAll (listOfNInts (101) -> "l") { l: List[Int] =>
           val test: Par[Int] = chooser[Int,Int] (lazyUnit (n))  (a => lazyUnit (l (a)))
           val result = Par.run[Int] (pool16) (test) 
           result should be (l(n))
         }
      }
    }

  }

  "Exercise 7" - {

    "choiceN returns the right computation" in {
      val test: Par[Int] = choiceN[Int] (lazyUnit[Int] (0)) (gadget.listPar)
      Par.run[Int] (pool5) (test) should be (1)
    }

    // This test is on hold, because the book does not discuss speculative
    // execution. Perhaps we'll add it at some point
    // "choosing a fast computation in choice N is fast" in {
    // }

    "choice returns the right computation" in {
      forAll ("b") { b: Boolean =>
        val test: Par[Boolean] = choice (lazyUnit (b)) (lazyUnit (true), lazyUnit (false))
        val result = Par.run[Boolean] (pool5) (test) 
        result should be (b)
      }
    }
    
    // This test is on hold, because the book does not discuss speculative
    // execution. Perhaps we'll add it at some point
    // "choice returns the fast computation faster" in {}
  }

  "Exercise 6" - {
    "map3 does not force the computation" in {
      map3 (gadget.parCrash, gadget.parCrash, gadget.parCrash) ((a,b,c) => 42)
    }

    "map3 does not wait eagerly for completion" in {
      gadget.reset
      val parResult: Par[Int] = map3 (gadget.parInt, gadget.parInt, gadget.parInt) ((a,b,c) => a+b+c)
      Par.run[Int] (pool5) (parResult)
      gadget.counter should be < 3
    }

    "map3 runs all arguments and fuses them correctly" in {
      val parResult: Par[Int] = map3 (gadget.parInt, gadget.parInt, gadget.parInt) ((a,b,c) => a-b/c)
      val test: Par[Int] = gadget.reset.assertLT (parResult, 5)
      val result = Par.run[Int] (pool5) (test)
      result should be (41)
    }

    "map3 is functionally equivalent to calling the f paramater without parallelization" in { 
      forAll ("a", "b", "c") { (a: Int, b: Int, c: Int) => 
        forAll ("f") { (f: (Int,Int,Int) => Int) =>
          val test = map3 (lazyUnit (a), lazyUnit (b), lazyUnit (c)) (f)
          val result = Par.run[Int] (pool5) (test)
          result should be (f (a,b,c))
        }
      }
    }

    "map3 obtains some parallelism" in { 
        forAll ("f") { (f: (Int,Int,Int) => Int) =>
          gadget.reset
          val test = map3 (gadget.parInt,gadget.parInt,gadget.parInt) (f)
          val result = Par.run[Int] (pool5) (test)
          gadget.counter should be < (3)
        }
    }

  }

  "Exercise 5" - {

    "parFilter does not force the computation" in {
      parFilter[Int] (List (1)) (n => ???)
    }

    "parFilter does not wait eagerly for completion" in {
      gadget.reset
      val list = List.fill (5) (42)
      val test = parFilter (list) (_ => true)
      Par.run (pool5) (test)
      gadget.counter should be < (5)
    }

    "parFilter tries all the elements" in {
      val list = List.fill (5) (42)
      val test = parFilter (list) (_ => true)
      val result = Par.run (pool5) (test)
      result should be (List (42,42,42,42,42))
    }

    "parFilter runs the elements in parallel" in {
      val list = List.fill (10) (42)
      val test = gadget.reset.assertLT (parFilter (list) (_ => true), 8)
      Par.run (pool16) (test)
    }

    "parFilter is functionally equivalent to filter" in {
      forAll (listOfNInts (200) -> "l") { l: List[Int] =>
        forAll ("f") { (f: Int => Boolean) =>
          val result = Par.run (pool16) (parFilter (l) (f))
          l filter f should be (result)
        }
      }
    }

  }


  "Exercise 4" - {

    "sequence does not force the computation" in {
      sequence (List (lazyUnit[Any] (???)))
    }

    "sequence does not wait eagerly for completion" in {
      gadget.reset
      val list = List.fill (10) (gadget.parInt)
      Par.run (pool2) (sequence (list))
      gadget.counter should be < (10)
    }

    "sequence does not loose any elements" in {
      val list = List.fill (5) (gadget.parInt)
      val test = gadget.reset.assertEqual (sequence (list), 5)
      Par.run (pool1) (test)
    }

    "sequence still runs elements in parallel" in {
      val list = List.fill (10) (gadget.parInt)
      val test = gadget.reset.assertLT (sequence (list), 5)
      Par.run (pool5) (test)
    }

    "sequence runs map in parallel (composes with map)" in {
      val list = sequence (List.fill (5) (gadget.parInt))
      val test = gadget.reset.assertLT (map (list) {_ => 42}, 4)
      Par.run (pool5) (test)
    }
  }

  "Exercise 2"  - {

    "asyncF is lazy"  in { asyncF[Unit,Unit] { _ => ??? } }

    "asyncF makes a function that runs in parallel" in {
      val test = gadget.reset.assertEqual (asyncF (gadget.task) (42), 0)
      Par.run (pool2) (test)
    }
  }

}
