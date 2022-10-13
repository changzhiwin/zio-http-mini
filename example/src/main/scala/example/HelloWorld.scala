package example

import zio._
import zio.http._
import zio.http.model.Method
import zio.http.model.HeaderNames

object HelloWorld extends ZIOAppDefault {

  def httpApp: HttpApp[Any, Nothing] = Http.collect[Request] {
    case Request(_, _, Method.GET, URL(path, _, queryParams, _), _, remoteAddress)  => {
      Response.json(s"""{"method": "GET", "path": "${path}", "name": "${queryParams.get("name")}", "remote": "${remoteAddress}"}""")
    }
  }

  // header的key默认小写，并且大小写敏感
  def httpZIOApp = Http.collectZIO[Request] {
    case Request(body, headers, Method.POST, _, _, _) => for {
      duration <- Random.nextLongBetween(1L, 6L)
      _        <- ZIO.unit.delay(duration.seconds)
      content  <- body.asString      
    } yield Response.json(s"""{"content-type": "${headers.get(HeaderNames.contentType.toString)}", "content": "${content}", "duration": "${duration}"}""")
  }

  def appPure = httpZIOApp.middleware(Middleware.timeout(3.seconds)).defaultWith( 
    httpApp.middleware(Middleware.runBefore {
      Console.printLine("I'm the runBefore middleware on httpApp")
    })
  )

  def appFinal = appPure.middleware(Middleware.debug)

  override def run = Server.serve(appFinal).provide(Server.default)
}