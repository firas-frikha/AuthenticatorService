ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.16"

lazy val root = (project in file("."))
  .settings(
    name := "AuthenticatorService"
  )

val AkkaVersion = "2.10.4"
val scalaTestVersion = "3.2.19"


resolvers += "Akka library repository".at("https://repo.akka.io/maven")

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-persistence-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-persistence-testkit" % AkkaVersion % Test,

  "com.typesafe.akka" %% "akka-serialization-jackson" % AkkaVersion,

  "com.typesafe.akka" %% "akka-persistence-testkit" % AkkaVersion % Test,

  "org.scalatest" %% "scalatest" % scalaTestVersion % Test


)