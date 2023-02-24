package sttp.client4

import sttp.client4.internal.InternalResponseAs

/** Abstract representation of how a response body should be handled.
  *
  * @tparam T
  *   Target type as which the response will be read.
  * @tparam R
  *   The backend capabilities required by the response description. This might be `Any` (no requirements), [[Effect]]
  *   (the backend must support the given effect type), [[Streams]] (the ability to send and receive streaming bodies)
  *   or [[WebSockets]] (the ability to handle websocket requests).
  */
trait AbstractResponseAs[+T, -R] {
  private[client4] def internal: InternalResponseAs[T, R]
  def show: String = internal.show
}
