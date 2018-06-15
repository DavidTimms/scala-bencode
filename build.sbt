lazy val commonSettings = Seq(
  organization := "net.mooli",
  scalaVersion := "2.12.6",
  scalacOptions in (Compile, doc) ++= Seq(
    "-diagrams",
    "-diagrams-dot-path", "/Users/abuse/bin/dot-hide-warnings",
    "-groups"
  ),
  libraryDependencies ++=
    "org.scalactic" %% "scalactic" % "3.0.5" % "test" ::
    "org.scalatest" %% "scalatest" % "3.0.5" % "test" ::
    Nil,
  scalaSource in Compile := ( baseDirectory(_ / "src") ).value,
  scalaSource in Test := ( baseDirectory(_ / "test") ).value,
  )

lazy val bencode = (project in file("."))
  .settings(commonSettings: _*)
