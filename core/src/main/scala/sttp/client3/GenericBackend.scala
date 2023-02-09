package sttp.client3

import sttp.monad.MonadError
import sttp.capabilities.Effect

/** A specific implementation of HTTP request sending logic.
  *
  * Instances of this trait shouldn't be used directly, if possible. Instead, implementations of [[Backend]] should be
  * used, which wrap instances of [[GenericBackend]] and provide a better developer experience.
  *
  * @note
  *   Backends should try to classify known HTTP-related exceptions into one of the categories specified by
  *   [[SttpClientException]]. Other exceptions are thrown unchanged.
  * @tparam F
  *   The effect type used when returning responses. E.g. [[Identity]] for synchronous backends,
  *   [[scala.concurrent.Future]] for asynchronous backends.
  * @tparam P
  *   Capabilities supported by this backend, in addition to [[Effect]]. This might be `Any` (no special capabilities),
  *   [[Streams]] (the ability to send and receive streaming bodies) or [[WebSockets]] (the ability to handle websocket
  *   requests).
  */
trait GenericBackend[F[_], +P] {

  def send[T](request: AbstractRequest[T, P with Effect[F]]): F[Response[T]]

  def close(): F[Unit]

  /** A monad instance for the effect type used when returning responses. Allows writing wrapper backends, which
    * map/flatMap over the return value of [[send]].
    */
  def responseMonad: MonadError[F]
}
