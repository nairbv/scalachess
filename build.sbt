
scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"


libraryDependencies += "org.slf4j" % "slf4j-api" % "1.6.1"

libraryDependencies += "com.weiglewilczek.slf4s" %% "slf4s" % "1.0.7"

libraryDependencies ++= Seq("ch.qos.logback" % "logback-core" % "0.9.30", "ch.qos.logback" % "logback-classic" % "0.9.30")


//below options are for configuring the jvm
//todo: use a Seq to make this more readable

fork in run := true

javaOptions in run +=  "-verbosegc"

javaOptions in run +=  "-XX:+PrintGCDetails"

javaOptions in run += "-verbosegc"

javaOptions in run += "-Xloggc:gc.log"

//in the "run" version, profile the cpu.  trying to find a way to get
//better depth in chess.

javaOptions in run += "-agentlib:hprof=cpu=times"

javaOptions in run += "-Xmx2G"

javaOptions in run += "-Xmn1G"

javaOptions in run += "-XX:MaxPermSize=256M"


javaOptions in console +=  "-verbosegc"

javaOptions in console +=  "-XX:+PrintGCDetails"

javaOptions in console += "-verbosegc"

javaOptions in console += "-Xloggc:gc.log"

javaOptions in console += "-Xmx2G"

javaOptions in console += "-Xmn1G"

javaOptions in console += "-XX:MaxPermSize=256M"


