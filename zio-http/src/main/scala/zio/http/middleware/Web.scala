package zio.http.middleware

import zio._
import zio.http._
import zio.http.model._

import java.io.{IOException}

private[zio] trait Web {

  final def debug: HttpMiddleware[Any, IOException] =
    new HttpMiddleware[Any, IOException] {
      override def apply[R, E >: IOException](app: HttpApp[R, E])(implicit trace: Trace): HttpApp[R, E] =
        Http.fromOptionFunction { request =>
          for {
            start    <- Clock.nanoTime
            response <- app(request)
            end      <- Clock.nanoTime
            _        <- Console
              .printLine(
                s"${response.status.asJava.code()} ${request.method} ${request.url.encode} ${(end - start) / 1000000}ms"
              )
              .asSomeError
          } yield response
        }
    }

  final def timeout(duration: Duration): HttpMiddleware[Any, Nothing] =
    new HttpMiddleware[Any, Nothing] {
      def apply[R, E](app: HttpApp[R, E])(implicit trace: Trace): HttpApp[R, E] =
        Http.fromOptionFunction { request =>
          for {
            response <- app(request).timeoutTo(Response.status(Status.RequestTimeout))(identity)(duration)
          } yield response
        }
    }

  final def runBefore[R, E](effect: ZIO[R, E, Any]): HttpMiddleware[R, E] =
    new HttpMiddleware[R, E] {
      override def apply[R1 <: R, E1 >: E](app: HttpApp[R1, E1])(implicit trace: Trace): HttpApp[R1, E1] =
        Http.fromOptionFunction { request =>
          for {
            _        <- effect.asSomeError
            response <- app(request)
          } yield response
        }
    }
}