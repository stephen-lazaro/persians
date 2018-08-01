name := "persians"

version := "1.0"

scalaVersion := "2.12.5"

scalacOptions ++= Seq (
  "-Xfatal-warnings",
  "-Xfuture",
  "-Xlint",
  "-Ypartial-unification",
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
  "org.typelevel" %% "cats-core" % "1.2.0",
  "org.typelevel" %% "cats-effect" % "1.0.0-RC2",
  "org.typelevel" %% "cats-free" % "1.2.0"
)
