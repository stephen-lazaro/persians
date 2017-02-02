name := "persians"

version := "1.0"

scalaVersion := "2.12.1"

scalacOptions ++= Seq (
  "-Xfatal-warnings",
  "-Xfuture",
  "-Xlint",
  "-Ywarn-unused-import",
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:higherKinds")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq (
  "org.typelevel" %% "cats" % "0.9.0",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.typelevel" %% "discipline" % "0.7.3",
  "org.scalatest" %% "scalatest" % "3.0.1"
)
