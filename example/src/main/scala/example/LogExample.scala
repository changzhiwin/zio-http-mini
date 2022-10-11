package example

import java.nio.file.{Path, Paths, Files}

import zio.logging.{Logger, LogFormat, LoggerTransport}

object LogExample {

  // val LogPath: Path = Files.createTempFile("logs", ".out");
  val LogPath: Path = Paths.get("./out.txt")
  val PropName = "ZIOHttpLogLevel"

  val tsList = List(
      LoggerTransport.file(LogPath).withFormat(LogFormat.inlineMaximus), 
      LoggerTransport.console.withFormat(LogFormat.inlineColored))
  val log = Logger(tsList).detectLevelFromProps(PropName).withTags("LogExample")

  def main(args: Array[String]): Unit = {

    log.info(s"args.length = ${args.length}")

    log.error(s"path = ${LogPath}")

  }
}