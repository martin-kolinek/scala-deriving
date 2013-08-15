import sbt._
import Keys._

object DerivingBuild extends Build {
	val buildSettings = Defaults.defaultSettings ++ Seq(
		name:="scala-deriving",
		version:="0.1",
		scalaVersion:="2.10.2",
		libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test",
		scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
	)
	
	lazy val main = Project("main", file("."), settings = buildSettings) dependsOn(macroSub)
	
	lazy val macroSub = Project("macro", file("macro")) settings (
			name:="scala-deriving-macro",
			scalaVersion:="2.10.2",
			scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
			libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.2") 
}