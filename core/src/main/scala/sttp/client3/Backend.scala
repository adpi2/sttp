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

//

trait EffectBackend[F[_]] extends Backend[F] { self =>
  override type Capabilities = Any
  override type SelfType = EffectBackend[F]

  override def wrap(f: GenericBackend[F, Any] => GenericBackend[F, Any]): EffectBackend[F] =
    new EffectBackend[F] {
      override type Capabilities = self.Capabilities
      override def genericBackend: GenericBackend[F, Capabilities] = f(self.genericBackend)
    }
}

object EffectBackend {
  def apply[F[_]](gb: GenericBackend[F, Any]): EffectBackend[F] = new EffectBackend[F] {
    override def genericBackend: GenericBackend[F, Any] = gb
  }
}

trait SyncBackend extends Backend[Identity] { self =>
  override type Capabilities = Any
  override type SelfType = SyncBackend

  override def wrap(f: GenericBackend[Identity, Any] => GenericBackend[Identity, Any]): SyncBackend = new SyncBackend {
    override type Capabilities = self.Capabilities
    override def genericBackend: GenericBackend[Identity, Capabilities] = f(self.genericBackend)
  }
}

object SyncBackend {
  def apply(gb: GenericBackend[Identity, Any]): SyncBackend = new SyncBackend {
    override def genericBackend: GenericBackend[Identity, Any] = gb
  }
}

trait StreamBackend[F[_], S] extends Backend[F] { self =>
  override type Capabilities = S
  override type SelfType = StreamBackend[F, S]

  override def wrap(f: GenericBackend[F, Capabilities] => GenericBackend[F, Capabilities]): StreamBackend[F, S] =
    new StreamBackend[F, S] {
      override type Capabilities = self.Capabilities
      override def genericBackend: GenericBackend[F, Capabilities] = f(self.genericBackend)
    }
}

object StreamBackend {
  def apply[F[_], S](gb: GenericBackend[F, S]): StreamBackend[F, S] = new StreamBackend[F, S] {
    override def genericBackend: GenericBackend[F, S] = gb
  }
}

trait WebSocketBackend[F[_]] extends Backend[F] { self =>
  override type Capabilities = WebSockets
  override type SelfType = WebSocketBackend[F]

  override def wrap(
      f: GenericBackend[F, Capabilities] => GenericBackend[F, Capabilities]
  ): WebSocketBackend[F] = new WebSocketBackend[F] {
    override type Capabilities = self.Capabilities
    override def genericBackend: GenericBackend[F, Capabilities] = f(self.genericBackend)
  }
}

object WebSocketBackend {
  def apply[F[_]](gb: GenericBackend[F, WebSockets]): WebSocketBackend[F] = new WebSocketBackend[F] {
    override def genericBackend: GenericBackend[F, WebSockets] = gb
  }
}

trait WebSocketStreamBackend[F[_], S] extends Backend[F] { self =>
  override type Capabilities = S with WebSockets
  override type SelfType = WebSocketStreamBackend[F, S]

  override def wrap(
      f: GenericBackend[F, Capabilities] => GenericBackend[F, Capabilities]
  ): WebSocketStreamBackend[F, S] = new WebSocketStreamBackend[F, S] {
    override type Capabilities = self.Capabilities
    override def genericBackend: GenericBackend[F, Capabilities] = f(self.genericBackend)
  }
}

object WebSocketStreamBackend {
  def apply[F[_], S](gb: GenericBackend[F, S with WebSockets]): WebSocketStreamBackend[F, S] =
    new WebSocketStreamBackend[F, S] {
      override def genericBackend: GenericBackend[F, S with WebSockets] = gb
    }
}
