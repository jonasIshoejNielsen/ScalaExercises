name := "lens"

scalaVersion := "2.13.3"

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

libraryDependencies += "com.github.julien-truffaut" %% "monocle-core"  % "2.0.3"
libraryDependencies += "com.github.julien-truffaut" %% "monocle-macro" % "2.0.3"
// libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"
// Switched to a milestone release, because we are using the instances of Traverse for Seq
// This should be changed after 2020 to a stable release
// https://github.com/typelevel/cats/pull/3620
libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0-M2"
