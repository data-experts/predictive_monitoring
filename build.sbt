lazy val commonSettings = Seq(
  organization := "de.huberlin.informatik",
  version := "1.0.0",
  scalaVersion := "2.11.8",
  EclipseKeys.withSource := true
)

lazy val root = 
	(project in file(".")).aggregate(process_prediction, feature_extraction)

lazy val feature_extraction = (project in file("feature_extraction")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= Seq(
	"ch.qos.logback" %  "logback-classic" % "1.1.7",
	"com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
	"nz.ac.waikato.cms.weka" % "weka-dev" % "3.9.0",
	"com.google.guava" % "guava" % "19.0",
	"de.vandermeer" % "asciitable" % "0.2.5",
	"org.scalactic" %% "scalactic" % "2.2.6",
	"org.scalatest" %% "scalatest" % "2.2.6" % "test"
  )
)
	
lazy val process_prediction = (project in file("process_prediction")).
  dependsOn(feature_extraction).
  settings(commonSettings: _*).
  settings(
	libraryDependencies ++= Seq(
	"tw.edu.ntu.csie" % "libsvm" % "3.17",
	"nz.ac.waikato.cms.weka" % "LibSVM" % "1.0.8",
	"com.github.wookietreiber" %% "scala-chart" % "latest.integration",
	"com.itextpdf" % "itextpdf" % "5.5.6",
	"org.jfree" % "jfreesvg" % "3.0",
	"com.github.tototoshi" %% "scala-csv" % "1.3.1"
   ))
