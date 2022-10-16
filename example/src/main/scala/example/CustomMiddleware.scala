package example

import zio._
import zio.http._
import zio.http.middleware._

object CustomMiddleware {

  def trace[R, E >: Throwable] = new HttpMiddleware[R, E] { // Middleware[R, E, Request, Response, Request, Response]

    //  def apply[R1 <: R, E1 >: E](http: Http[R1, E1, AIn, BIn])(implicit trace: Trace): Http[R1, E1, AOut, BOut]
    // 经验：override需要保持类型参数，否则无效；
    override def apply[R1 <: R, E1 >: E](http: HttpApp[R1, E1])(implicit trace: Trace): HttpApp[R1, E1] = {

      http
        .contramapZIO[R1, E1, Request] { request =>
        
          for {
            _ <- Console.printLine(s"> ${request.method} | ${request.path} | ${request.version}")
            _ <- ZIO.foreachDiscard(request.headers.toList) { h =>
                   Console.printLine(s"> ${h._1}: ${h._2}")
                 }
          } yield request
        }
        .mapZIO[R1, E1, Response] { response =>
          for {
            -    <- Console.printLine(s"< ${response.status}")
            _    <- ZIO.foreachDiscard(response.headers.toList) { h =>
                      Console.printLine(s"< ${h._1}: ${h._2}")
                    }
            body <- response.body.asString
            _    <- Console.printLine(s"< ${body}").orDie
          } yield response
        }
    }
  }
}

