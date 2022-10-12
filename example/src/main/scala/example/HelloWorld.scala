package example

import java.util.concurrent.TimeUnit

import zio._
import zio.http._
import zio.http.model.Method
import zio.http.model.HeaderNames

object HelloWorld extends ZIOAppDefault {

  def httpApp: HttpApp[Any, Nothing] = Http.collect[Request] {
    case Request(_, _, Method.GET, URL(path, _, queryParams, _), _, remoteAddress)  => {

      Response.json(s"""{"method": "GET", "path": "${path}", "query": "${queryParams.encode}", "remote": "${remoteAddress}"}""")
    }
    case Request(body, headers, Method.POST, _, _, _)          => {

      Response.json(s"""{"header": "${headers.get("X-Test-H")}", "isComplete": "${body.isComplete}"}""")
    }
  }

  def httpZIOApp = Http.collectZIO[Request] {

    case Request(_, _, Method.GET, URL(path, _, queryParams, _), _, remoteAddress)  => for {
      now <- Clock.currentTime(TimeUnit.MILLISECONDS)
    } yield Response.json(s"""{"name":"${queryParams.get("name")}", "now": "${now}"}""")

    case Request(body, headers, Method.POST, _, _, _)          => for {
      content <- body.asString      
    } yield Response.json(s"""{"type": "${headers.get(HeaderNames.contentType.toString)}", "content": "${content}"}""")
  }

  override def run = Server.serve(httpZIOApp).provide(Server.default)
}