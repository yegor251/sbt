scalaVersion := "3.3.1"
name := "my-zio-project"
version := "0.0.1"
javacOptions ++= Seq("-source", "22", "-target", "22")


libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % "2.0.21",
)
libraryDependencies += "dev.zio" %% "zio-http" % "3.0.0-RC3"
libraryDependencies ++= Seq(
  "org.scalikejdbc" %% "scalikejdbc" % "4.2.1",
  "com.h2database" % "h2" % "1.4.200",
  "ch.qos.logback" % "logback-classic" % "1.2.12"
)
libraryDependencies += "org.postgresql" % "postgresql" % "42.3.1"
libraryDependencies += "com.typesafe" % "config" % "1.4.1"
libraryDependencies += "dev.zio" %% "zio-json" % "0.6.2"
libraryDependencies += "com.typesafe" % "config" % "1.4.1"