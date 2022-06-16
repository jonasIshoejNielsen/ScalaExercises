// wasowski, Advanced Programming, IT University of Copenhagen

class MyModuleSpec extends org.scalatest.freespec.AnyFreeSpec 
    with org.scalatest.matchers.should.Matchers
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  "square" - {

    "should not throw exceptions" in {
      forAll ("n") { (n: Int) => 
        noException should be thrownBy MyModule.square (n) }
    }

    // does not test for overflow, but this is not the point here
    "behaves like n*n" in {
      forAll ("n") { (n: Int) => MyModule.square (n) should be (n*n) }
    }

  }

}
