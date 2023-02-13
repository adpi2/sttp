package sttp.client3

import sttp.capabilities.WebSockets

/** A specific implementation of HTTP request sending logic.
  *
  * Requests can be sent using their `.send` method, e.g. [[Request.send]]. Depending on the capabilities required by a
  * request, a backend of an appropriate type should be used, that is one of: [[SyncBackend]], [[EffectBackend]],
  * [[StreamBackend]], [[WebSocketBackend]], [[WebSocketStreamBackend]].
  *
  * @note
  *   Backends should try to classify known HTTP-related exceptions into one of the categories specified by
  *   [[SttpClientException]]. Other exceptions are thrown unchanged.
  *
  * @note
  *   The [[Backend]] trait and its implementations wrap instances of [[GenericBackend]] (where the actual logic
  *   resides). These wrappers aim to provide a better developer experience by improving code completion and type
  *   inference, as backends with various capabilities (websockets, streaming, effects) are represented as top-level
  *   types, instead of type parameters (as in [[GenericBackend]]).
  *
  * @tparam F
  *   The effect type used to represent side-effects. E.g. [[Identity]] for synchronous backends,
  *   [[scala.concurrent.Future]] for asynchronous backends.
  */
trait Backend[F[_]] { self =>
  type Capabilities
  type SelfType <: Backend[F]
  def genericBackend: GenericBackend[F, Capabilities]
  def close(): F[Unit] = genericBackend.close()

  def wrap(f: GenericBackend[F, Capabilities] => GenericBackend[F, Capabilities]): SelfType
}

object Backend {
  def apply[F[_]](gb: GenericBackend[F, Any]): Backend[F] = new Backend[F] {
    override type Capabilities = Any
    override type SelfType = Backend[F]
    override def genericBackend: GenericBackend[F, Any] = gb
    override def wrap(f: GenericBackend[F, Any] => GenericBackend[F, Any]): Backend[F] = apply(f(genericBackend))
  }
}

trait SyncBackend extends Backend[Identity] { self =>
  override type Capabilities <: Any
  override type SelfType <: SyncBackend
}

object SyncBackend {
  def apply(gb: GenericBackend[Identity, Any]): SyncBackend = new SyncBackend {
    override type Capabilities = Any
    override type SelfType = SyncBackend
    override def genericBackend: GenericBackend[Identity, Any] = gb

    override def wrap(f: GenericBackend[Identity, Any] => GenericBackend[Identity, Any]): SyncBackend = apply(
      f(genericBackend)
    )
  }
}

trait StreamBackend[F[_], S] extends Backend[F] { self =>
  override type Capabilities <: S
  override type SelfType <: StreamBackend[F, S]
}

object StreamBackend {
  def apply[F[_], S](gb: GenericBackend[F, S]): StreamBackend[F, S] = new StreamBackend[F, S] {
    override type Capabilities = S
    override type SelfType = StreamBackend[F, S]
    override def genericBackend: GenericBackend[F, S] = gb

    override def wrap(f: GenericBackend[F, S] => GenericBackend[F, S]): StreamBackend[F, S] = apply(f(genericBackend))
  }
}

trait WebSocketBackend[F[_]] extends Backend[F] { self =>
  override type Capabilities <: WebSockets
  override type SelfType <: WebSocketBackend[F]
}

object WebSocketBackend {
  def apply[F[_]](gb: GenericBackend[F, WebSockets]): WebSocketBackend[F] = new WebSocketBackend[F] {
    override type Capabilities = WebSockets
    override type SelfType = WebSocketBackend[F]
    override def genericBackend: GenericBackend[F, WebSockets] = gb

    override def wrap(f: GenericBackend[F, WebSockets] => GenericBackend[F, WebSockets]): WebSocketBackend[F] = apply(
      f(genericBackend)
    )
  }
}

trait WebSocketStreamBackend[F[_], S] extends WebSocketBackend[F] with StreamBackend[F, S] { self =>
  override type Capabilities <: S with WebSockets
  override type SelfType <: WebSocketStreamBackend[F, S]
}

object WebSocketStreamBackend {
  def apply[F[_], S](gb: GenericBackend[F, S with WebSockets]): WebSocketStreamBackend[F, S] =
    new WebSocketStreamBackend[F, S] {
      override type Capabilities = S with WebSockets
      override type SelfType = WebSocketStreamBackend[F, S]
      override def genericBackend: GenericBackend[F, S with WebSockets] = gb

      override def wrap(
          f: GenericBackend[F, S with WebSockets] => GenericBackend[F, S with WebSockets]
      ): WebSocketStreamBackend[F, S] = apply(
        f(genericBackend)
      )
    }
}
