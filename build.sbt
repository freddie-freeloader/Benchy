scalaVersion := "2.11.7"
enablePlugins(JmhPlugin)
libraryDependencies ++= Seq(
   "com.lihaoyi" %% "fastparse" % "0.4.2" ,
   "com.slamdata" %% "matryoshka-core" % "0.17.0"
)
