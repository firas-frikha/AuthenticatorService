ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.16"

enablePlugins(ScoverageSbtPlugin)

lazy val root = (project in file("."))
  .settings(
    name := "AuthenticatorService"
  )

val akkaVersion = "2.10.4"
val scalaTestVersion = "3.2.19"
val scalaMockVersion = "7.4.1"
val akkaProjectionVersion = "1.6.14"


resolvers += "Akka library repository".at("https://repo.akka.io/maven")

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-persistence-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-persistence-testkit" % akkaVersion % Test,

  "com.typesafe.akka" %% "akka-serialization-jackson" % akkaVersion,

  "com.typesafe.akka" %% "akka-persistence-testkit" % akkaVersion % Test,

  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "org.scalamock" %% "scalamock" % scalaMockVersion % Test,


  "com.lightbend.akka" %% "akka-projection-core" % akkaProjectionVersion,
  "com.lightbend.akka" %% "akka-projection-eventsourced" % akkaProjectionVersion

)