package sttp.client3

import sttp.capabilities.Effect
import sttp.model.Uri
import sttp.monad.syntax._

class ResolveRelativeUrisBackend[F[_], P](delegate: GenericBackend[F, P], resolve: Uri => F[Uri])
    extends DelegateSttpBackend(delegate) {

  override def send[T](request: AbstractRequest[T, P with Effect[F]]): F[Response[T]] = {
    val request2 = if (request.uri.isRelative) {
      resolve(request.uri).map { uri2 =>
        request.method(method = request.method, uri = uri2)
      }
    } else request.unit

    request2.flatMap(delegate.send)
  }
}

object ResolveRelativeUrisBackend {
  def apply[F[_]](delegate: Backend[F], baseUri: Uri): delegate.SelfType =
    delegate.wrap(
      new ResolveRelativeUrisBackend(_, uri => delegate.genericBackend.responseMonad.eval(baseUri.resolve(uri)))
    )
}
