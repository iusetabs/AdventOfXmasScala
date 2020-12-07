val commonSettings: Def.SettingsDefinition = Seq(
    version := "0.1",
    scalaVersion := "2.13.4"
  )

lazy val common = (project in file("shared"))
  .settings(
    commonSettings,
    name := "adventofxmas-shared",
  )

lazy val main = (project in file("."))
  .settings(
      commonSettings,
      name := "adventofxmas",
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  ).dependsOn(common)
