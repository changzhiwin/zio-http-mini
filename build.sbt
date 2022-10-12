import BuildHelper._
import Dependencies._
import sbt.librarymanagement.ScalaArtifacts.isScala3

// Setting default log level to INFO
val _ = sys.props += ("ZIOHttpLogLevel" -> Debug.ZIOHttpLogLevel)

lazy val root = (project in file("."))
  .settings(stdSettings("root"))
  //.settings(publishSetting(false))
  .aggregate(
    zioHttp,
    //zioHttpBenchmarks,
    zioHttpLogging,
    //zioHttpExample,
  )

lazy val zioHttp = (project in file("zio-http"))
  .settings(stdSettings("zio-http"))
  //.settings(publishSetting(true))
  //.settings(meta)
  .settings(
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    libraryDependencies ++= netty ++ Seq(
      `zio`,
      `zio-streams`,
      `zio-schema`,
      `zio-schema-json`,
      `zio-test`,
      `zio-test-sbt`,
      `netty-incubator`,
    ),
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n <= 12 => Seq(`scala-compact-collection`)
        case _                       => Seq.empty
      }
    },
  )
  .dependsOn(zioHttpLogging)

lazy val zioHttpLogging = (project in file("zio-http-logging"))
  .settings(stdSettings("zio-http-logging"))
  //.settings(publishSetting(false))
  .settings(
    libraryDependencies ++= {
      if (isScala3(scalaVersion.value)) Seq.empty
      else Seq(reflect.value % Provided)
    },
  )
  .settings(
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    libraryDependencies ++= Seq(`zio`, `zio-test`, `zio-test-sbt`),
  )

lazy val example = (project in file("example"))
  .settings(stdSettings("example"))
  //.settings(publishSetting(false))
  .settings(runSettings(Debug.Main))
  //.settings(libraryDependencies ++= Seq(`jwt-core`))
  .dependsOn(zioHttpLogging, zioHttp)