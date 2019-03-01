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

/////////// Projects ///////////

lazy val root = (project in file("."))
  .settings(
    name := "timejur",
  )
  .aggregate(timejurBasic, timejurTypelevel)

lazy val timejurBasic = (project in file("basic"))
  .settings(
    name := "timejur-basic",
    commonScala2Settings,
  )

lazy val timejurTypelevel = (project in file("typelevel"))
  .settings(
    name := "timejur-iz",
    commonScala2Settings,
    libraryDependencies ++= Seq(
      "eu.timepit" %% "refined" % "0.9.4",
    ),
  )
  .dependsOn(izCore)

lazy val izCore = (project in file("iz-core"))
  .settings(
    name := "iz-core",
    commonScala2Settings,
    libraryDependencies ++= Seq(
      "eu.timepit" %% "singleton-ops" % "0.3.1",
    ),
  )

// todo to add izLaws as soon as available and to make izCore to depend on (izLaws % "test").
