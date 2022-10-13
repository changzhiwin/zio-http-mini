package zio.http

import zio._
import zio.http.middleware.{MonoMiddleware, Web}

trait Middleware[-R, +E, +AIn, -BIn, -AOut, +BOut] { self =>

  def apply[R1 <: R, E1 >: E](http: Http[R1, E1, AIn, BIn])(implicit trace: Trace): Http[R1, E1, AOut, BOut]
}

object Middleware extends Web{

  def identity[A, B]: MonoMiddleware[Any, Nothing, A, B] = Identity

  //def identity[AIn, BIn, AOut, BOut](implicit ev: IsMono[AIn, BIn, AOut, BOut]): Middleware[Any, Nothing, AIn, BIn, AOut, BOut] = Identity


  private object Identity extends Middleware[Any, Nothing, Nothing, Any, Any, Nothing] {
    override def apply[R1 <: Any, E1 >: Nothing](http: Http[R1, E1, Nothing, Any])(implicit
      trace: Trace,
    ): Http[R1, E1, Any, Nothing] =
      http.asInstanceOf[Http[R1, E1, Any, Nothing]]
  }
}