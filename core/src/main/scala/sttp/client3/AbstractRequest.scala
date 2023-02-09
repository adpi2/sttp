package sttp.client3

import sttp.capabilities.Effect
import sttp.client3.internal.ToCurlConverter
import sttp.client3.internal.ToRfc2616Converter
import sttp.model.Header
import sttp.model.Method
import sttp.model.RequestMetadata
import sttp.model.Uri

import scala.collection.immutable.Seq

/** Abstract representation of an HTTP request.
  *
  * The request can be sent using a [[GenericBackend]] which provides a superset of the required capabilities.
  *
  * @tparam T
  *   The target type, to which the response body should be read.
  * @tparam R
  *   The backend capabilities required by the request or response description. This might be `Any` (no requirements),
  *   [[sttp.capabilities.Effect]] (the backend must support the given effect type), [[sttp.capabilities.Streams]] (the
  *   ability to send and receive streaming bodies) or [[sttp.capabilities.WebSockets]] (the ability to handle websocket
  *   requests).
  */
trait AbstractRequest[T, -R] extends RequestBuilder[AbstractRequest[T, R]] {
  def method: Method
  def uri: Uri
  def body: AbstractBody[R]
  def response: AbstractResponseAs[T, R]
  def mapResponse[T2](f: T => T2): AbstractRequest[T2, R]

  def toCurl: String = ToCurlConverter(this)
  def toCurl(sensitiveHeaders: Set[String]): String =
    ToCurlConverter(this, sensitiveHeaders)

  def toRfc2616Format: String = ToRfc2616Converter.requestToRfc2616(this)
  def toRfc2616Format(sensitiveHeaders: Set[String]): String =
    ToRfc2616Converter.requestToRfc2616(this, sensitiveHeaders)

  private[client3] def onlyMetadata: RequestMetadata = {
    val m = method
    val u = uri
    val h = headers
    new RequestMetadata {
      override val method: Method = m
      override val uri: Uri = u
      override val headers: Seq[Header] = h
    }
  }

  def isWebSocket: Boolean = (this: Any) match {
    case _: WebSocketRequest[_, _]       => true
    case _: WebSocketStreamRequest[_, _] => true
    case _                               => false
  }

  /** Sends the request, using the given generic backend. If possible, requests should be sent using one of the
    * higher-level `send` methods which accept a [[Backend]] implementation, e.g. [[Request.send]].
    *
    * The required capabilities must be a subset of the capabilities provided by the backend.
    *
    * @return
    *   An `F`-effect, containing a [[Response]], with the body handled as specified by this request (see
    *   [[Request.response]]). Effects might include asynchronous computations (e.g. [[scala.concurrent.Future]]), pure
    *   effect descriptions (`IO`), or error wrappers (see [[EitherBackend]] and [[TryBackend]]). Exceptions are
    *   represented as failed effects (e.g. failed futures).
    *
    * Known exceptions are converted by backends to one of [[SttpClientException]]. Other exceptions are thrown
    * unchanged.
    */
  def send[F[_], P](backend: GenericBackend[F, P])(implicit
      pEffectFIsR: P with Effect[F] <:< R
  ): F[Response[T]] =
    backend.send(this.asInstanceOf[AbstractRequest[T, P with Effect[F]]]) // as witnessed by pEffectFIsR
}
