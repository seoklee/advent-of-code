name := "scala"

version := "1.4.4"

scalaVersion := "2.13.4"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "4.10.5" % "test")

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"

scalacOptions in Test ++= Seq("-Yrangepos")