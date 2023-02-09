package sttp.client3

import sttp.client3.monad.MapEffect
import sttp.capabilities.Effect
import sttp.monad.MonadError
import sttp.client3.monad.FunctionK

class MappedEffectBackend[F[_], G[_], P](
    backend: GenericBackend[F, P],
    f: FunctionK[F, G],
    g: FunctionK[G, F],
    m: MonadError[G]
) extends GenericBackend[G, P] {
  override def send[T](request: AbstractRequest[T, P with Effect[G]]): G[Response[T]] =
    f(backend.send(MapEffect[G, F, T, P](request, g, f, m, backend.responseMonad)))

  override def close(): G[Unit] = f(backend.close())

  override def responseMonad: MonadError[G] = m
}

object MappedEffectBackend {
//  def apply[F[_], G[_]](
//      backend: Backend[F],
//      f: FunctionK[F, G],
//      g: FunctionK[G, F],
//      m: MonadError[G]
//  ): Backend[G] = new EffectBackend[G] {
//    override type Capabilities = backend.Capabilities
//    override def genericBackend: GenericBackend[G, Capabilities] =
//      new MappedEffectBackend[F, G, Capabilities](backend.genericBackend, f, g, m)
//  }
//
//  def apply[F[_], G[_], S](
//      backend: StreamBackend[F, S],
//      f: FunctionK[F, G],
//      g: FunctionK[G, F],
//      m: MonadError[G]
//  ): StreamBackend[G, S] = new StreamBackend[G, S] {
//    override def genericBackend: GenericBackend[G, S] =
//      new MappedEffectBackend[F, G, S](backend.genericBackend, f, g, m)
//  }
}
