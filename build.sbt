val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Adts",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.2",
    libraryDependencies +=  "com.disneystreaming" %% "weaver-cats" % "0.8.3" % Test,
    testFrameworks += new TestFramework("weaver.framework.CatsEffect")
  )
