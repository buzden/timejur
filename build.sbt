ThisBuild / version := "0.1"

lazy val catsVersion = "1.6.0"
lazy val specs2Version = "4.4.1"

lazy val commonScala2Settings = Seq(
  // General stuff
  scalaVersion := "2.13.0-M5",
  scalacOptions ++= Seq(
    "-language:higherKinds",
  ),

  libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion,
  libraryDependencies += compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9"),
  libraryDependencies += compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0-M4"),

  // Data types stuff
  libraryDependencies ++= Seq(
  ),

  // Testing stuff
  libraryDependencies ++= Seq(
    "org.specs2" %% "specs2-scalacheck" % specs2Version,
//    "io.chrisdavenport" %% "cats-scalacheck" % "0.1.0", // todo to enable when this supports 2.13
    "org.typelevel" %% "cats-laws" % catsVersion,
    "org.typelevel" %% "discipline" % "0.11.0", // todo to remove as soon as this or newer comes as dep.
  ).map(_ % Test),
  scalacOptions in Test += "-Yrangepos",
  testOptions   in Test += Tests.Argument("showtimes"),
  logBuffered   in Test := false,
)

lazy val commonTypelevelSettings = Seq(
  // Typelevel stuff
  libraryDependencies ++= Seq(
    "eu.timepit" %% "singleton-ops" % "0.3.1",
    "eu.timepit" %% "refined" % "0.9.4",
  ),
)

