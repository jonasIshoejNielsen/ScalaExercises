name := "lens"

scalaVersion := "2.13.5"

scalacOptions ++= Seq (
  "-deprecation", 
  "-feature", 
  "-Xfatal-warnings", 
  "-Ymacro-annotations"
)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" 
libraryDependencies += "org.scalatest" %% "scalatest-freespec" % "3.2.0" 
libraryDependencies += "org.scalatest" %% "scalatest-shouldmatchers" % "3.2.0" 
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-14" % "3.2.0.0" 

libraryDependencies += "dev.optics" %% "monocle-core"  % "3.0.0"
libraryDependencies += "dev.optics" %% "monocle-macro" % "3.0.0"
// libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"
// Switched to a milestone release, because we are using the instances of Traverse for Seq
// This should be changed after 2020 to a stable release
// https://github.com/typelevel/cats/pull/3620
libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0-M2"

// It is easier to read the results if they don't run in parallel.
Test / parallelExecution := false // true is the default in sbt, 
