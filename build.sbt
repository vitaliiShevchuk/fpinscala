name := "fpinscala"

version := "0.1"

scalaVersion := "2.13.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"

coverageExcludedPackages += "part2.*, part3.Chapter11*"