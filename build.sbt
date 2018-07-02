name := "safe-matrix"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

/*scalacOptions ++= Seq(
  "-Xlog-implicit-conversions",
  "-Xlog-implicits")*/

testOptions in Test := Seq(
  Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1")
)