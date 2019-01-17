name := "BBCScalaTrainingPlayground"

version := "0.1"

scalaVersion := "2.12.8"


lazy val scalaReflect = Def.setting { "org.scala-lang" % "scala-reflect" % scalaVersion.value }
libraryDependencies += scalaReflect.value