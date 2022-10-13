package zio.http

// import io.netty.handler.codec.http.HttpHeaderNames
import zio.ZIO.attemptBlocking
import zio._
import zio.http.model._
import zio.http.model.headers.HeaderModifierZIO

// import java.io.{File, FileNotFoundException}
import java.net

/**
 * A functional domain to model Http apps using ZIO and that can work over any
 * kind of request and response types.
 */
sealed trait Http[-R, +E, -A, +B] { self =>

  import Http._

  /**
   * Named alias for `>>>`
   */
  final def andThen[R1 <: R, E1 >: E, B1 >: B, C](other: Http[R1, E1, B1, C]): Http[R1, E1, A, C] =
    Http.Chain(self, other)

  /**
   * Named alias for `++` 
   */
  final def defaultWith[R1 <: R, E1 >: E, A1 <: A, B1 >: B](other: Http[R1, E1, A1, B1]): Http[R1, E1, A1, B1] =
    Http.Combine(self, other)

  /**
    * Named alias for `@@`
    */
  final def middleware[R1 <: R, E1 >: E, A1 <: A, B1 >: B, A2, B2](
    mid: Middleware[R1, E1, A1, B1, A2, B2]
  ): Http[R1, E1, A2, B2] = Http.RunMiddleware(self, mid)

  final def foldCauseHttp[R1 <: R, E1, A1 <: A, C1](
    failure: Cause[E] => Http[R1, E1, A1, C1],
    success: B => Http[R1, E1, A1, C1],
    empty: Http[R1, E1, A1, C1],
  ): Http[R1, E1, A1, C1] =
    Http.FoldHttp(self, failure, success, empty)

  /**
   * Transforms the output of the http app
   */
  final def map[C](bc: B => C): Http[R, E, A, C] = self.flatMap(b => Http.succeed(bc(b)))

  /**
   * Transforms the output of the http effectfully
   */
  final def mapZIO[R1 <: R, E1 >: E, C](bFc: B => ZIO[R1, E1, C])(implicit trace: Trace): Http[R1, E1, A, C] =
    self.andThen(Http.fromFunctionZIO(bFc))

  /**
   * Transforms the failure of the http app
   */
  final def mapError[E1](ee: E => E1): Http[R, E1, A, B] =
    self.foldHttp(e => Http.fail(ee(e)), Http.succeed, Http.empty)

  /**
   * Creates a new Http app from another
   */
  final def flatMap[R1 <: R, E1 >: E, A1 <: A, C1](f: B => Http[R1, E1, A1, C1]): Http[R1, E1, A1, C1] = {
    self.foldHttp(Http.fail, f, Http.empty)
  }

  /**
   * Translates app failure into death of the app, making all failures unchecked
   * and not a part of the type of the app.
   */
  final def orDie(implicit ev1: E <:< Throwable, ev2: CanFail[E]): Http[R, Nothing, A, B] =
    orDieWith(ev1)

  /**
   * Keeps none of the errors, and terminates the http app with them, using the
   * specified function to convert the `E` into a `Throwable`.
   */
  final def orDieWith(f: E => Throwable)(implicit ev: CanFail[E]): Http[R, Nothing, A, B] =
    self.foldHttp(e => Http.die(f(e)), Http.succeed, Http.empty)

  /**
   * Flattens an Http app of an Http app
   */
  final def flatten[R1 <: R, E1 >: E, A1 <: A, B1](implicit
    ev: B <:< Http[R1, E1, A1, B1],
  ): Http[R1, E1, A1, B1] = {
    self.flatMap(scala.Predef.identity(_))
  }

  /**
   * Folds over the http app by taking in two functions one for success and one
   * for failure respectively.
   */
  final def foldHttp[R1 <: R, A1 <: A, E1, B1](
    failure: E => Http[R1, E1, A1, B1],
    success: B => Http[R1, E1, A1, B1],
    empty: Http[R1, E1, A1, B1],
  ): Http[R1, E1, A1, B1] =
    foldCauseHttp(c => c.failureOrCause.fold(failure, Http.failCause(_)), success, empty)

  /**
   * Consumes the input and executes the Http.
   */
  final def apply(a: A)(implicit trace: Trace): ZIO[R, Option[E], B] = execute(a).toZIO

  /**
   * Evaluates the app and returns an HExit that can be resolved further
   *
   * NOTE: `execute` is not a stack-safe method for performance reasons. Unlike
   * ZIO, there is no reason why the execute should be stack safe. The
   * performance improves quite significantly if no additional heap allocations
   * are required this way.
   */
  final private[zio] def execute(a: A)(implicit trace: Trace): HExit[R, E, B] =
    self match {

      case Http.Empty                              => HExit.empty
      case Http.Identity                           => HExit.succeed(a.asInstanceOf[B])
      case Succeed(b)                              => HExit.succeed(b)
      case Fail(cause)                             => HExit.failCause(cause)
      case Attempt(a)                              =>
        try { HExit.succeed(a()) }
        catch { case e: Throwable => HExit.fail(e.asInstanceOf[E]) }
      case FromFunctionHExit(f)                    =>
        try { f(a) }
        catch { case e: Throwable => HExit.die(e) }
      case FromHExit(h)                            => h
      case Chain(self, other)                      => self.execute(a).flatMap(b => other.execute(b))
      case FoldHttp(self, failure, success, empty) =>
        try {
          self.execute(a).foldExit(failure(_).execute(a), success(_).execute(a), empty.execute(a))
        } catch {
          case e: Throwable => HExit.die(e)
        }
      case RunMiddleware(app, mid) =>
        try {
          mid(app).execute(a)
        } catch {
          case e: Throwable => HExit.die(e)
        }
      case Combine(self, other) => 
        self.execute(a) match {
          case HExit.Empty            => other.execute(a)
          case exit: HExit.Success[_] => exit.asInstanceOf[HExit[R, E, B]]
          case exit: HExit.Failure[_] => exit.asInstanceOf[HExit[R, E, B]]
          // TOOD, why can't use exit: HExit.Effect[_, _, _]
          case exit @ HExit.Effect(_) => exit.defaultWith(other.execute(a)).asInstanceOf[HExit[R, E, B]]
        }
    }
}

object Http {

  implicit final class HttpAppSyntax[-R, +E](val http: HttpApp[R, E]) extends HeaderModifierZIO[HttpApp[R, E]] {
    self =>

    /**
     * Updates the response headers using the provided function
     */
    // TODO 没有理解，http上没有定义updateHeaders方法啊？？
    // 理解了：http的输出是Response，map操作作用在Response上面！！
    override def updateHeaders(update: Headers => Headers)(implicit trace: Trace): HttpApp[R, E] =
      http.map(_.updateHeaders(update))
  }

  def text(charSeq: CharSequence): HttpApp[Any, Nothing] =
    Http.succeed(Response.text(charSeq))

  def apply[B](b: B): Http[Any, Nothing, Any, B] = Http.succeed(b)

  def fromZIO[R, E, B](effect: ZIO[R, E, B])(implicit trace: Trace): Http[R, E, Any, B] =
    Http.fromFunctionZIO(_ => effect)

  def getResource(path: String)(implicit trace: Trace): Http[Any, Throwable, Any, net.URL] =
    Http
      .fromZIO(attemptBlocking(getClass.getClassLoader.getResource(path)))
      .flatMap { resource => if (resource == null) Http.empty else Http.succeed(resource) }

  /**
   * Returns an http app that dies with the specified `Throwable`. This method
   * can be used for terminating an app because a defect has been detected in
   * the code. Terminating an http app leads to aborting handling of an HTTP
   * request and responding with 500 Internal Server Error.
   */
  def die(t: Throwable): UHttp[Any, Nothing] =
    failCause(Cause.die(t))

  def fail[E](e: E): Http[Any, E, Any, Nothing] =
    failCause(Cause.fail(e))

  ////////////////////////////// Create case class

  def attempt[A](a: => A): Http[Any, Throwable, Any, A] = Attempt(() => a)

  def empty: Http[Any, Nothing, Any, Nothing] = Http.Empty

  def failCause[E](cause: Cause[E]): Http[Any, E, Any, Nothing] = Http.Fail(cause)

  def fromHExit[R, E, B](h: HExit[R, E, B]): Http[R, E, Any, B] = FromHExit(h)

  def identity[A]: Http[Any, Nothing, A, A] = Http.Identity

  def succeed[B](b: B): Http[Any, Nothing, Any, B] = Http.Succeed(b)

  ////////////////////////////// Create case class end

  /**
   * Creates an HTTP app which accepts a request and produces response.
   */
  def collect[A]: Http.PartialCollect[A] = Http.PartialCollect(())

  /**
   * Create an HTTP app from a partial function from A to HExit[R,E,B]
   */
  def collectHExit[A]: Http.PartialCollectHExit[A] = Http.PartialCollectHExit(())

  /**
   * Create an HTTP app from a partial function from A to Http[R,E,A,B]
   */
  def collectHttp[A]: Http.PartialCollectHttp[A] = Http.PartialCollectHttp(())

  /**
   * Creates an HTTP app which accepts a request and produces response
   * effectfully.
   */
  def collectZIO[A]: Http.PartialCollectZIO[A] = Http.PartialCollectZIO(())

  // 这里相当于把pf做了一个转换：
  // 从 PartialFunction[A, ZIO[R, E, B]]  
  // 到 PartialFunction[A, Http[R, E, A, B]]
  // 这样就和PartialCollectHttp是一样了
  final case class PartialCollectZIO[A](unit: Unit) extends AnyVal {
    def apply[R, E, B](pf: PartialFunction[A, ZIO[R, E, B]])(implicit trace: Trace): Http[R, E, A, B] =
      Http.collect[A] { case a if pf.isDefinedAt(a) => Http.fromZIO(pf(a)) }.flatten
  }

  final case class PartialCollect[A](unit: Unit) extends AnyVal {
    def apply[B](pf: PartialFunction[A, B]): Http[Any, Nothing, A, B] = {
      FromFunctionHExit(
        pf.lift(_) match {
          case Some(value) => HExit.succeed(value)
          case None        => HExit.Empty
        }
      )
    }
  }

  // 为什么需要flatten?
  // 只有这种场景：Http[R, E, A, Http[R, E, A, B]]
  // pf被lift之后，如果成功返回的类型 FromFunctionHExit(a => HExit.succeed( Http[R, E, A, B] ) )
  final case class PartialCollectHttp[A](unit: Unit) extends AnyVal {
    def apply[R, E, B](pf: PartialFunction[A, Http[R, E, A, B]]): Http[R, E, A, B] =
      Http.collect[A](pf).flatten
  }

  final case class PartialCollectHExit[A](unit: Unit) extends AnyVal {
    def apply[R, E, B](pf: PartialFunction[A, HExit[R, E, B]]): Http[R, E, A, B] =
      FromFunctionHExit(a => if (pf.isDefinedAt(a)) pf(a) else HExit.empty)
  }

  //////////////////// FromFunction

  /**
   * Creates a Http from a pure function
   */
  def fromFunction[A]: PartialFromFunction[A] = new PartialFromFunction[A](())

  /**
   * Creates a Http from an pure function from A to HExit[R,E,B]
   */
  def fromFunctionHExit[A]: PartialFromFunctionHExit[A] = new PartialFromFunctionHExit[A](())

  /**
   * Creates a Http from an effectful pure function
   */
  def fromFunctionZIO[A]: PartialFromFunctionZIO[A] = new PartialFromFunctionZIO[A](())

  /**
   * Creates an `Http` from a function that takes a value of type `A` and
   * returns with a `ZIO[R, Option[E], B]`. The returned effect can fail with a
   * `None` to signal "not found" to the backend.
   */
  def fromOptionFunction[A]: PartialFromOptionFunction[A] = new PartialFromOptionFunction(())

  final class PartialFromOptionFunction[A](val unit: Unit) extends AnyVal {
    def apply[R, E, B](f: A => ZIO[R, Option[E], B])(implicit trace: Trace): Http[R, E, A, B] = Http
      .collectZIO[A] { case a =>
        f(a)
          .map(Http.succeed)
          .catchAll {
            case Some(error) => ZIO.succeed(Http.fail(error))
            case None        => ZIO.succeed(Http.empty)
          }
          .catchAllDefect(defect => ZIO.succeed(Http.die(defect)))
      }
      .flatten
  }

  final class PartialFromFunction[A](val unit: Unit) extends AnyVal {
    def apply[B](f: A => B): Http[Any, Nothing, A, B] = Http.identity[A].map(f)
  }

  final class PartialFromFunctionZIO[A](val unit: Unit) extends AnyVal {
    def apply[R, E, B](f: A => ZIO[R, E, B])(implicit trace: Trace): Http[R, E, A, B] =
      FromFunctionHExit(a => HExit.fromZIO(f(a)))
  }

  final class PartialFromFunctionHExit[A](val unit: Unit) extends AnyVal {
    def apply[R, E, B](f: A => HExit[R, E, B]): Http[R, E, A, B] = FromFunctionHExit(f)
  }

  //////////////////// Define case class

  private final case class Succeed[B](b: B) extends Http[Any, Nothing, Any, B]

  private final case class Fail[E](cause: Cause[E]) extends Http[Any, E, Any, Nothing]

  private final case class FromFunctionHExit[R, E, A, B](f: A => HExit[R, E, B]) extends Http[R, E, A, B]

  private final case class Chain[R, E, A, B, C](self: Http[R, E, A, B], other: Http[R, E, B, C])
      extends Http[R, E, A, C]

  private final case class FoldHttp[R, E, EE, A, B, BB](
    self: Http[R, E, A, B],
    failure: Cause[E] => Http[R, EE, A, BB],
    success: B => Http[R, EE, A, BB],
    empty: Http[R, EE, A, BB],
  ) extends Http[R, EE, A, BB]

  private case class Attempt[A](a: () => A) extends Http[Any, Nothing, Any, A]

  private final case class FromHExit[R, E, B](h: HExit[R, E, B]) extends Http[R, E, Any, B]

  private case object Empty extends Http[Any, Nothing, Any, Nothing]

  private case object Identity extends Http[Any, Nothing, Any, Nothing]

  // 在defaultWith方法里面做了类型约束：
  // EE 必须是 E 的父类或本身
  // BB 必须是 B 的父类或本身
  private final case class Combine[R, E, EE, A, B, BB](
    self: Http[R, E, A, B],
    other: Http[R, EE, A, BB],
  ) extends Http[R, EE, A, BB]

  private final case class RunMiddleware[R, E, A1, B1, A2, B2](
    http: Http[R, E, A1, B1],
    mid: Middleware[R, E, A1, B1, A2, B2]
  ) extends Http[R, E, A2, B2]
}
