package sttp.client3.logging

import java.util.concurrent.TimeUnit
import sttp.client3._
import sttp.monad.syntax._

import scala.concurrent.duration.Duration
import sttp.capabilities.Effect

class LoggingWithResponseBodyBackend[F[_], P](
    delegate: GenericBackend[F, P],
    log: Log[F],
    includeTiming: Boolean
) extends DelegateSttpBackend[F, P](delegate) {

  private def now(): Long = System.currentTimeMillis()
  private def elapsed(from: Option[Long]): Option[Duration] = from.map(f => Duration(now() - f, TimeUnit.MILLISECONDS))

  override def send[T](request: AbstractRequest[T, P with Effect[F]]): F[Response[T]] = {
    log.beforeRequestSend(request).flatMap { _ =>
      val start = if (includeTiming) Some(now()) else None
      def sendAndLog(request: AbstractRequest[(T, Option[String]), P with Effect[F]]): F[Response[T]] = {
        for {
          r <- delegate.send(request)
          _ <- log.response(request, r, r.body._2, elapsed(start))
        } yield r.copy(body = r.body._1)
      }
      val response = request match {
        case request: Request[T] =>
          sendAndLog(request.response(asBothOption(request.response, asStringAlways)))
        case request: StreamRequest[T, P with Effect[F]] =>
          sendAndLog(request.response(asBothOption(request.response, asStringAlways)))
        case request =>
          for {
            r <- delegate.send(request)
            _ <- log.response(request, r, None, elapsed(start))
          } yield r
      }
      response.handleError { case e: Exception =>
        log
          .requestException(request, elapsed(start), e)
          .flatMap(_ => responseMonad.error(e))
      }
    }
  }
}

object LoggingWithResponseBodyBackend {
  def apply[F[_]](backend: Backend[F], log: Log[F], includeTiming: Boolean): backend.SelfType =
    backend.wrap(new LoggingWithResponseBodyBackend(_, log, includeTiming))
}
