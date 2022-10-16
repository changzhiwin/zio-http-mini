package zio.http

import zio.http.Path.Segment

private[zio] trait PathSyntax {

  val !! : Path = Path.root 

  val ~~ : Path = Path.empty

  object /: {
    def unapply(path: Path): Option[(String, Path)] =
      for {
        head <- path.segments.headOption.map {
          case Segment.Text(text) => text
          case Segment.Root       => ""
        }
        tail = path.segments.drop(1)
      } yield (head, Path(tail))
  }

  object / {
    def unapply(path: Path): Option[(Path, String)] = {
      path.segments match {
        case Nil                        => None
        case Nil :+ Segment.Root        => Some(~~ -> "")
        case Nil :+ Segment.Text(text)  => Some(~~ -> text)
        case head :+ Segment.Root       => Some(Path(head) -> "")
        case head :+ Segment.Text(text) => Some(Path(head) -> text )
        case _                          => None // If not, not all missing cases are reported
      }
    }
  }
}