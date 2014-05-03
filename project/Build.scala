import sbt._
import Keys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys

object DerivingBuild extends Build {
	val buildSettings = Defaults.defaultSettings ++ Seq(
		name:="scala-deriving",
                organization:="org.kolinek",
		version:="0.2.0",
		scalaVersion:="2.11.0",
		libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.5" % "test",
		scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ymacro-debug-lite")
	)
	
	lazy val main = Project("main", file("."), settings = buildSettings) aggregate(macroSub) dependsOn(macroSub)
	
	lazy val macroSub = Project("macro", file("macro")) settings (
			name:="scala-deriving-macro",
                        organization:="org.kolinek",
                        version:="0.1",
			scalaVersion:="2.11.0",
			scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
			libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.0",
                        EclipseKeys.skipParents in ThisBuild := false)
}
