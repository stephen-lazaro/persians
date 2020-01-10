name := "persians"

version := "1.0"

scalaVersion := "2.12.5"

scalacOptions ++= Seq (
  //"-Xfatal-warnings",
  "-Xfuture",
  "-Xlint",
  "-Ypartial-unification",
  //"-Ywarn-unused-import",
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:higherKinds")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq (
  "org.typelevel" %% "cats-core" % "1.2.0",
  "org.typelevel" %% "cats-effect" % "1.0.0-RC2",
  "org.typelevel" %% "cats-free" % "1.2.0",
  "com.github.finagle" %% "finchx-core" % "0.28.0",
  "org.typelevel" %% "cats-tagless-core" % "0.6",
  "org.typelevel" %% "cats-tagless-macros" % "0.6",
  "com.github.to-ithaca" %% "libra" % "0.5.0",
  "org.typelevel" %% "spire" % "0.17.0-M1",
  "eu.timepit" %% "refined" % "0.9.5",
  "io.frees" %% "iota-core" % "0.3.10"
)
