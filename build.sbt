name := "Hybrid Programming"

version := "1.0"

scalaVersion := "2.13.4" //"2.12.10" // 2.11.8  2.12.4

// resolver for scala tests.
resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"

// more warnings
scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")

// more complete check for complete "cases" (scala 2.10)
// initialize ~= { _ => sys.props("scalac.patmat.analysisBudget") = "512" }

libraryDependencies ++= Seq(

    "junit" % "junit" % "4.12"
  , "com.novocode" % "junit-interface" % "0.11" % "test" // to run junit tests from sbt

  // , "org.choco-solver" % "choco-solver" % "4.0.6"
  // , "org.scala-lang" % "scala-compiler" % scalaVersion.value // needed for JIT
  , "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2" // needed for newers scala versions
  /// new parser
  , "org.typelevel" %% "cats-parse" % "0.3.0"
  //
  , "org.scalactic" %% "scalactic" % "3.2.7"
  , "org.scalatest" %% "scalatest" % "3.2.7" % "test"

  // Last stable release
  ,"org.scalanlp" %% "breeze" % "1.1",
  // Native libraries are not included by default. add this if you want them (as of 0.7)
  // Native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "1.1",
  // The visualization library is distributed separately as well.
  // It depends on LGPL code
  "org.scalanlp" %% "breeze-viz" % "1.1",

  // Optimus to solve quadratic programming problem
  "com.github.vagmcs" %% "optimus" % "3.2.4",
  "com.github.vagmcs" %% "optimus-solver-oj" % "3.2.4"
)

// tests cannot be run in parallel, because of the Choco solver
parallelExecution in Test := false
