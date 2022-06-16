// wasowski, Advanced Programming, IT University of Copenhagen
package adpro.variance

// Use sbt command: testOnly adpro.variance.VarianceSpec to see the exception
//
// The test is written in Scala for convenience, but we are testing a problem in
// Java, and running a Java function

class VarianceSpec extends org.scalatest.freespec.AnyFreeSpec {

  /* The test is ignored to avoid confusion, as the expected result is a
     failure. If you want to run it, please replace 'ignore' with 'in'.  */

  "Runtime crash caused by covariance of arrays" ignore {

    /* Intentionally produces java.lang.ArrayStoreException
       (to demonstrate the problem with covariance of arrays in Java) */
    adpro.variance.Variance.problem()

  }

}
