package example

import zio._
import zio.http._
import zio.http.model.Method

object RouteSyntax extends ZIOAppDefault {

  def httpZIOApp = Http.collectZIO[Request] {
    
    // http://localhost:8080/abc/say
    case Method.GET -> !! / "abc" / lang  => for {
      star <- Random.nextLongBetween(1L, 11L)
    } yield Response.json(s"""{"star": "${star}", "lang": "${lang}"}""")

    // http://localhost:8080/abc/say/
    case Method.GET -> !! / "abc" / full / "" => for {
      star <- Random.nextLongBetween(1L, 11L)
    } yield Response.json(s"""{"star": "${star}", "full": "${full}"}""")

    case Method.GET -> "" /: who /: "say" /: ~~ => for {
      sound <- Random.nextLongBetween(1L, 101L)
    } yield Response.json(s"""{"who": "${who}", "sound": "${sound}"}""")
  }

  override def run = Server.serve(httpZIOApp).provide(Server.default)

}