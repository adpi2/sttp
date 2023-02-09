package sttp.client3.listener

import sttp.client3._
import sttp.monad.syntax._
import sttp.capabilities.Effect

/** A backend wrapper which notifies the given [[RequestListener]] when a request starts and completes. */
class ListenerBackend[F[_], P, L](
    delegate: GenericBackend[F, P],
    listener: RequestListener[F, L]
) extends DelegateSttpBackend(delegate) {
  override def send[T](request: AbstractRequest[T, P with Effect[F]]): F[Response[T]] = {
    listener.beforeRequest(request).flatMap { t =>
      responseMonad
        .handleError(delegate.send(request)) { case e: Exception =>
          listener.requestException(request, t, e).flatMap(_ => responseMonad.error(e))
        }
        .flatMap { response => listener.requestSuccessful(request, response, t).map(_ => response) }
    }
  }
}

object ListenerBackend {
  def lift[F[_], L](delegate: Backend[F], listener: RequestListener[Identity, L]): delegate.SelfType =
    apply(delegate, RequestListener.lift(listener, delegate.genericBackend.responseMonad))

  def apply[F[_], L](delegate: Backend[F], listener: RequestListener[F, L]): delegate.SelfType =
    delegate.wrap(new ListenerBackend(_, listener))
}
