name := "monad"

scalaVersion := "2.13.3"

scalacOptions ++= Seq ("-deprecation", "-feature", "-Xfatal-warnings")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" 
libraryDependencies += "org.scalatest" %% "scalatest-freespec" % "3.2.0" 
libraryDependencies += "org.scalatest" %% "scalatest-shouldmatchers" % "3.2.0" 
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-14" % "3.2.0.0" 
// Used for overriding equality on functions:
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.0"

// We have multiple specs. 
// It is easier to read the results if they don't run in parallel.
parallelExecution in Test := false // true is the default in sbt, 
