name := "ScalazPlayground"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.4",
  "org.specs2" %% "specs2-core" % "3.6.4" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")
