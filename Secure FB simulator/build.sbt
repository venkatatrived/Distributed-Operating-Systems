lazy val commonSettings = Seq(
name := "Facebook API",
version := "1.1",
scalaVersion := "2.11.7",
resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
resolvers += "spray repo" at "http://repo.spray.io",
//logLevel := Level.Warn,
//scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
    val akkaV = "2.3.9"
    val sprayV = "1.3.3"
    Seq(
        "io.spray"            %%  "spray-can"     % sprayV,
        "io.spray"            %%  "spray-client"     % sprayV,
        "io.spray"            %%  "spray-routing" % sprayV,
        "io.spray"            %%  "spray-testkit" % sprayV  % "test",
        "io.spray"              %%  "spray-json" % "1.3.2",
        "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
        "com.typesafe.akka"   %%  "akka-testkit"  % akkaV   % "test",
        "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
        "com.typesafe.akka" %% "akka-slf4j"      % akkaV,
        "ch.qos.logback"    %  "logback-classic" % "1.1.2",
        "net.debasishg" %% "redisclient" % "3.0"
       )
},
Revolver.settings
)

lazy val root = (project in file(".")).
  aggregate(server, client)

lazy val server = project.settings(commonSettings: _*).settings(
  name := "Facebook API server"
)

lazy val client = project.dependsOn(server).settings(commonSettings: _*).settings(
  name := "Facebook API client"
)
