
scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"


libraryDependencies += "org.slf4j" % "slf4j-api" % "1.6.1"

libraryDependencies += "com.weiglewilczek.slf4s" %% "slf4s" % "1.0.7"

libraryDependencies ++= Seq("ch.qos.logback" % "logback-core" % "0.9.30", "ch.qos.logback" % "logback-classic" % "0.9.30")




