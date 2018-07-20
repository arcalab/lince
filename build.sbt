name := "Hybrid Programming"

version := "1.0"

scalaVersion := "2.12.4" // 2.11.8  2.12.4

// resolver for scala tests.
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

// more warnings
scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")

// more complete check for complete "cases" (scala 2.10)
// initialize ~= { _ => sys.props("scalac.patmat.analysisBudget") = "512" }

libraryDependencies ++= Seq(

    "junit" % "junit" % "4.12"
  , "com.novocode" % "junit-interface" % "0.11" % "test" // to run junit tests from sbt

  // , "org.choco-solver" % "choco-solver" % "4.0.6"
  // , "org.scala-lang" % "scala-compiler" % scalaVersion.value // needed for JIT
  , "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6" // needed for newers scala versions
// 
  , "org.scalactic" %% "scalactic" % "3.0.4"
  , "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)

// tests cannot be run in parallel, because of the Choco solver
parallelExecution in Test := false
