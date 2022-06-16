name := "intro"

scalaVersion := "2.13.5"

scalacOptions ++= Seq ("-deprecation", "-feature", "-Xfatal-warnings")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest-freespec" % "3.2.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest-shouldmatchers" % "3.2.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest-mustmatchers" % "3.2.0" % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-14" % "3.2.0.0" % "test"
