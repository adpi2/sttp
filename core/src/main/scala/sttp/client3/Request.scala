package sttp.client3

import sttp.model._

import scala.collection.immutable.Seq
import sttp.capabilities.Effect
import sttp.client3.internal.{ToCurlConverter, ToRfc2616Converter}

trait RequestOps[+Req] {
  def toRequest(uri: Uri, method: Method): Req

  def get(uri: Uri): Req = toRequest(uri = uri, method = Method.GET)
  def head(uri: Uri): Req = toRequest(uri = uri, method = Method.HEAD)
  def post(uri: Uri): Req = toRequest(uri = uri, method = Method.POST)
  def put(uri: Uri): Req = toRequest(uri = uri, method = Method.PUT)
  def delete(uri: Uri): Req = toRequest(uri = uri, method = Method.DELETE)
  def options(uri: Uri): Req = toRequest(uri = uri, method = Method.OPTIONS)
  def patch(uri: Uri): Req = toRequest(uri = uri, method = Method.PATCH)
  def method(method: Method, uri: Uri): Req = toRequest(uri = uri, method = method)
}

/** Describes a HTTP request, along with a description of how the response body should be handled.
  *
  * The request can be sent using a [[SttpBackend]], which provides a superset of the required capabilities.
  *
  * @param response
  *   Description of how the response body should be handled. Needs to be specified upfront so that the response is
  *   always consumed and hence there are no requirements on client code to consume it. An exception to this are unsafe
  *   streaming and websocket responses, which need to be consumed/closed by the client.
  * @param tags
  *   Request-specific tags which can be used by backends for logging, metrics, etc. Not used by default.
  * @tparam T
  *   The target type, to which the response body should be read.
  * @tparam R
  *   The backend capabilities required by the request or response description. This might be `Any` (no requirements),
  *   [[Effect]] (the backend must support the given effect type), [[Streams]] (the ability to send and receive
  *   streaming bodies) or [[sttp.capabilities.WebSockets]] (the ability to handle websocket requests).
  */
case class Request[T, -R](
    method: Method,
    uri: Uri,
    body: RequestBody[R],
    headers: Seq[Header],
    response: ResponseAs[T, R],
    options: RequestOptions,
    tags: Map[String, Any]
) extends HasHeaders
    with PartialRequestOps[T, R, Request]
    with RequestOps[Request[T, R]] {

  override def methodOpt: Option[Method] = Some(method)

  override def uriOpt: Option[Uri] = Some(uri)

  override def copyWith[T2, R2](
      body: RequestBody[R2],
      headers: Seq[Header],
      response: ResponseAs[T2, R2],
      options: RequestOptions,
      tags: Map[String, Any]
  ): Request[T2, R2] =
    copy(body = body, headers = headers, response = response, options = options, tags = tags)

  def toRequest(uri: Uri, method: Method): Request[T, R] =
    copy[T, R](uri = uri, method = method)

  /** Sends the request, using the backend from the implicit scope. Only requests for which the method & URI are
    * specified can be sent.
    *
    * The required capabilities must be a subset of the capabilities provided by the backend.
    *
    * @return
    *   For synchronous backends (when the effect type is [[Identity]]), [[Response]] is returned directly and
    *   exceptions are thrown. For asynchronous backends (when the effect type is e.g. [[scala.concurrent.Future]]), an
    *   effect containing the [[Response]] is returned. Exceptions are represented as failed effects (e.g. failed
    *   futures).
    *
    * The response body is deserialized as specified by this request (see [[Request.response]]).
    *
    * Known exceptions are converted by backends to one of [[SttpClientException]]. Other exceptions are thrown
    * unchanged.
    */
  @deprecated(message = "use request.send(backend), providing the backend explicitly", since = "3.0.0")
  def send[F[_], P]()(implicit
      pEffectFIsR: P with Effect[F] <:< R,
      backend: SttpBackend[F, P]
  ): F[Response[T]] =
    send(backend)(pEffectFIsR) // the order of implicits must be different so that the signatures are different

  /** Sends the request, using the given backend. Only requests for which the method & URI are specified can be sent.
    *
    * The required capabilities must be a subset of the capabilities provided by the backend.
    *
    * @return
    *   For synchronous backends (when the effect type is [[Identity]]), [[Response]] is returned directly and
    *   exceptions are thrown. For asynchronous backends (when the effect type is e.g. [[scala.concurrent.Future]]), an
    *   effect containing the [[Response]] is returned. Exceptions are represented as failed effects (e.g. failed
    *   futures).
    *
    * The response body is deserialized as specified by this request (see [[Request.response]]).
    *
    * Known exceptions are converted by backends to one of [[SttpClientException]]. Other exceptions are thrown
    * unchanged.
    */
  def send[F[_], P](backend: SttpBackend[F, P])(implicit
      pEffectFIsR: P with Effect[F] <:< R
  ): F[Response[T]] = backend.send(this.asInstanceOf[Request[T, P with Effect[F]]]) // as witnessed by pEffectFIsR

  def toCurl: String = ToCurlConverter.requestToCurl(this)
  def toCurl(sensitiveHeaders: Set[String]): String =
    ToCurlConverter.requestToCurl(this, sensitiveHeaders)

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
}

object Request {
  implicit class RichRequestTEither[A, B, R](r: Request[Either[A, B], R]) {
    def mapResponseRight[B2](f: B => B2): Request[Either[A, B2], R] = r.copy(response = r.response.mapRight(f))
    def responseGetRight: Request[B, R] = r.copy(response = r.response.getRight)
  }

  implicit class RichRequestTEitherResponseException[HE, DE, B, R](
      r: Request[Either[ResponseException[HE, DE], B], R]
  ) {
    def responseGetEither: Request[Either[HE, B], R] = r.copy(response = r.response.getEither)
  }
}
