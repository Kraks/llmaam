val scala3Version = "3.7.0"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

lazy val root = project
  .in(file("."))
  .settings(
    name := "LLMAAM",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "dev.langchain4j" % "langchain4j" % "1.0.1" % Compile,
    libraryDependencies += "dev.langchain4j" % "langchain4j-open-ai" % "1.0.1" % Compile,
    libraryDependencies += "dev.langchain4j" % "langchain4j-google-ai-gemini" % "1.0.1-beta6" % Compile,
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.17" % Runtime,
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    libraryDependencies ++= Seq("org.scala-lang" %% "toolkit" % "0.2.0", "org.scala-lang" %% "toolkit-test" % "0.2.0" % Test)
  )
