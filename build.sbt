val scala3Version = "3.7.0"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

lazy val root = project
  .in(file("."))
  .settings(
    name := "LLMAAM",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "dev.langchain4j" % "langchain4j" % "1.24.0" % Compile,
    libraryDependencies += "dev.langchain4j" % "langchain4j-open-ai" % "1.24.0" % Compile,
    libraryDependencies += "dev.langchain4j" % "langchain4j-google-ai-gemini" % "1.24.0" % Compile,
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.17" % Runtime,
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    libraryDependencies ++= Seq("org.scala-lang" %% "toolkit" % "0.2.0", "org.scala-lang" %% "toolkit-test" % "0.2.0" % Test),
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
  )

val commonJavaOptions = Seq(
  "-Xms4G",
  "-Xmx32G",
  "-Xss1024M",
  "-XX:MaxMetaspaceSize=8G",
  "-XX:ReservedCodeCacheSize=2048M"
)
run / javaOptions ++= commonJavaOptions
Test / javaOptions ++= commonJavaOptions

// Tests reset the shared frontend fresh-name counter.
Test / parallelExecution := false
