lazy val commonSettings = Seq(
  organization := "net.mooli",
  scalaVersion := "3.2.1",
  Compile / doc / scalacOptions ++= Seq(
    "-diagrams",
    "-diagrams-dot-path", "/Users/abuse/bin/dot-hide-warnings",
    "-groups",
  ),
  libraryDependencies ++= List(
    "org.scalactic" %% "scalactic" % "3.2.14", // % "Test",
    "org.scalatest" %% "scalatest" % "3.2.14", // % "Test",
  ),
  Compile / scalaSource := (baseDirectory(_ / "src")).value,
  Test / scalaSource := (baseDirectory(_ / "test")).value,
)

lazy val bencode = (project in file("."))
  .settings(commonSettings: _*)
