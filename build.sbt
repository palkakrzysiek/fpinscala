lazy val dependencies =
  new {
    val typesafeConfigV = "1.3.1"
    val pureconfigV     = "0.8.0"
    val monocleV        = "1.4.0"
    val akkaV           = "2.5.6"
    val scalatestV      = "3.0.5"
    val scalacheckV     = "1.13.5"

    val typesafeConfig = "com.typesafe"               % "config"                   % typesafeConfigV
    val akka           = "com.typesafe.akka"          %% "akka-stream"             % akkaV
    val monocleCore    = "com.github.julien-truffaut" %% "monocle-core"            % monocleV
    val monocleMacro   = "com.github.julien-truffaut" %% "monocle-macro"           % monocleV
    val pureconfig     = "com.github.pureconfig"      %% "pureconfig"              % pureconfigV
    val scalatest      = "org.scalatest"              %% "scalatest"               % scalatestV
    val scalacheck     = "org.scalacheck"             %% "scalacheck"              % scalacheckV
  }

val commonSettings = Seq(
  scalaVersion := "2.12.1"
)

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises",
    libraryDependencies += dependencies.scalatest
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
  )

