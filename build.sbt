import com.typesafe.sbt.SbtNativePackager.autoImport.NativePackagerHelper._

val Http4sVersion  = "0.21.0-M5"
val Specs2Version  = "4.8.0"
val LogbackVersion = "1.2.3"
val CirceVersion   = "0.13.0"

lazy val root = (project in file("."))
  .settings(
    organization := "com.jenovs",
    name := "server",
    version := Http4sVersion,
    scalaVersion := "2.13.1",
    libraryDependencies ++= Seq(
      "org.http4s"     %% "http4s-blaze-server"  % Http4sVersion,
      "org.http4s"     %% "http4s-dsl"           % Http4sVersion,
      "org.specs2"     %% "specs2-core"          % Specs2Version % "test",
      "ch.qos.logback" % "logback-classic"       % LogbackVersion,
      "io.circe"       %% "circe-core"           % CirceVersion,
      "io.circe"       %% "circe-generic"        % CirceVersion,
      "io.circe"       %% "circe-generic-extras" % CirceVersion,
      "io.circe"       %% "circe-parser"         % CirceVersion
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3"),
    addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1"),
    mappings in Universal ++= directory(baseDirectory.value / "static"),
    buildInfoKeys := Seq[BuildInfoKey](name, version),
    buildInfoPackage := "com.jenovs.server",
    turbo := true
  )
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(BuildInfoPlugin)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature",
  "-Xfatal-warnings"
)

// https://sheltered-fortress-60020.herokuapp.com/
herokuAppName in Compile := "sheltered-fortress-60020"
