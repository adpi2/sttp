package sttp.client3

import sttp.client3.internal.{SttpFile, _}
import sttp.client3.logging.LoggingOptions
import sttp.model._
import sttp.model.headers.CookieWithMeta

import java.io.InputStream
import java.nio.ByteBuffer
import scala.collection.immutable.Seq
import scala.concurrent.duration.Duration
import sttp.capabilities.Streams
import java.util.Base64

trait PartialRequestOps[T, -R, PReq[X, -Y] <: PartialRequestOps[X, Y, PReq]] extends PartialRequestExt[T, R, PReq] {
  self: PReq[T, R] =>
  def methodOpt: Option[Method]
  def uriOpt: Option[Uri]
  def body: RequestBody[R]
  def headers: Seq[Header]
  def response: ResponseAs[T, R]
  def options: RequestOptions
  def tags: Map[String, Any]
  def copyWith[T2, R2](
      body: RequestBody[R2] = body,
      headers: Seq[Header] = headers,
      response: ResponseAs[T2, R2] = response,
      options: RequestOptions = options,
      tags: Map[String, Any] = tags
  ): PReq[T2, R2]

  def contentType(ct: String): PReq[T, R] =
    header(HeaderNames.ContentType, ct, replaceExisting = true)
  def contentType(mt: MediaType): PReq[T, R] =
    header(HeaderNames.ContentType, mt.toString, replaceExisting = true)
  def contentType(ct: String, encoding: String): PReq[T, R] =
    header(HeaderNames.ContentType, contentTypeWithCharset(ct, encoding), replaceExisting = true)
  def contentLength(l: Long): PReq[T, R] =
    header(HeaderNames.ContentLength, l.toString, replaceExisting = true)

  /** Adds the given header to the end of the headers sequence.
    * @param replaceExisting
    *   If there's already a header with the same name, should it be dropped?
    */
  def header(h: Header, replaceExisting: Boolean = false): PReq[T, R] = {
    val current = if (replaceExisting) headers.filterNot(_.is(h.name)) else headers
    copyWith(headers = current :+ h)
  }

  /** Adds the given header to the end of the headers sequence.
    * @param replaceExisting
    *   If there's already a header with the same name, should it be dropped?
    */
  def header(k: String, v: String, replaceExisting: Boolean): PReq[T, R] =
    header(Header(k, v), replaceExisting)

  /** Adds the given header to the end of the headers sequence. */
  def header(k: String, v: String): PReq[T, R] = header(Header(k, v))

  /** Adds the given header to the end of the headers sequence, if the value is defined. Otherwise has no effect. */
  def header(k: String, ov: Option[String]): PReq[T, R] =
    ov.fold(this: PReq[T, R])(header(k, _))
  def headers(hs: Map[String, String]): PReq[T, R] =
    headers(hs.map(t => Header(t._1, t._2)).toSeq: _*)
  def headers(hs: Header*): PReq[T, R] = copyWith(headers = headers ++ hs)
  def auth: SpecifyAuthScheme[T, R, PReq] =
    new SpecifyAuthScheme[T, R, PReq](HeaderNames.Authorization, this, DigestAuthenticationBackend.DigestAuthTag)
  def proxyAuth: SpecifyAuthScheme[T, R, PReq] =
    new SpecifyAuthScheme[T, R, PReq](
      HeaderNames.ProxyAuthorization,
      this,
      DigestAuthenticationBackend.ProxyDigestAuthTag
    )
  def acceptEncoding(encoding: String): PReq[T, R] =
    header(HeaderNames.AcceptEncoding, encoding, replaceExisting = true)

  def cookie(nv: (String, String)): PReq[T, R] = cookies(nv)
  def cookie(n: String, v: String): PReq[T, R] = cookies((n, v))
  def cookies(r: Response[_]): PReq[T, R] = cookies(
    r.cookies.collect { case Right(c) => c }.map(c => (c.name, c.value)): _*
  )
  def cookies(cs: Iterable[CookieWithMeta]): PReq[T, R] = cookies(cs.map(c => (c.name, c.value)).toSeq: _*)
  def cookies(nvs: (String, String)*): PReq[T, R] = {
    header(
      HeaderNames.Cookie,
      (headers.find(_.name == HeaderNames.Cookie).map(_.value).toSeq ++ nvs.map(p => p._1 + "=" + p._2)).mkString("; "),
      replaceExisting = true
    )
  }

  /** Uses the `utf-8` encoding.
    *
    * If content type is not yet specified, will be set to `text/plain` with `utf-8` encoding.
    *
    * If content length is not yet specified, will be set to the number of bytes in the string using the `utf-8`
    * encoding.
    */
  def body(b: String): PReq[T, R] = body(b, Utf8)

  /** If content type is not yet specified, will be set to `text/plain` with the given encoding.
    *
    * If content length is not yet specified, will be set to the number of bytes in the string using the given encoding.
    */
  def body(b: String, encoding: String): PReq[T, R] =
    withBody(StringBody(b, encoding))
      .setContentLengthIfMissing(b.getBytes(encoding).length.toLong)

  /** If content type is not yet specified, will be set to `application/octet-stream`.
    *
    * If content length is not yet specified, will be set to the length of the given array.
    */
  def body(b: Array[Byte]): PReq[T, R] =
    withBody(ByteArrayBody(b))
      .setContentLengthIfMissing(b.length.toLong)

  /** If content type is not yet specified, will be set to `application/octet-stream`.
    */
  def body(b: ByteBuffer): PReq[T, R] =
    withBody(ByteBufferBody(b))

  /** If content type is not yet specified, will be set to `application/octet-stream`.
    */
  def body(b: InputStream): PReq[T, R] =
    withBody(InputStreamBody(b))

  /** If content type is not yet specified, will be set to `application/octet-stream`.
    *
    * If content length is not yet specified, will be set to the length of the given file.
    */
  private[client3] def body(f: SttpFile): PReq[T, R] =
    withBody(FileBody(f)).setContentLengthIfMissing(f.size)

  /** Encodes the given parameters as form data using `utf-8`. If content type is not yet specified, will be set to
    * `application/x-www-form-urlencoded`.
    *
    * If content length is not yet specified, will be set to the length of the number of bytes in the url-encoded
    * parameter string.
    */
  def body(fs: Map[String, String]): PReq[T, R] =
    formDataBody(fs.toList, Utf8)

  /** Encodes the given parameters as form data. If content type is not yet specified, will be set to
    * `application/x-www-form-urlencoded`.
    *
    * If content length is not yet specified, will be set to the length of the number of bytes in the url-encoded
    * parameter string.
    */
  def body(fs: Map[String, String], encoding: String): PReq[T, R] =
    formDataBody(fs.toList, encoding)

  /** Encodes the given parameters as form data using `utf-8`. If content type is not yet specified, will be set to
    * `application/x-www-form-urlencoded`.
    *
    * If content length is not yet specified, will be set to the length of the number of bytes in the url-encoded
    * parameter string.
    */
  def body(fs: (String, String)*): PReq[T, R] =
    formDataBody(fs.toList, Utf8)

  /** Encodes the given parameters as form data. If content type is not yet specified, will be set to
    * `application/x-www-form-urlencoded`.
    *
    * If content length is not yet specified, will be set to the length of the number of bytes in the url-encoded
    * parameter string.
    */
  def body(fs: Seq[(String, String)], encoding: String): PReq[T, R] =
    formDataBody(fs, encoding)

  def multipartBody[R2](ps: Seq[Part[RequestBody[R2]]]): PReq[T, R with R2] =
    copyWith(body = MultipartBody(ps))

  def multipartBody[R2](p1: Part[RequestBody[R2]], ps: Part[RequestBody[R2]]*): PReq[T, R with R2] =
    copyWith(body = MultipartBody(p1 :: ps.toList))

  def streamBody[S](s: Streams[S])(b: s.BinaryStream): PReq[T, R with S] =
    withBody(StreamBody(s)(b))

  /** When the request is sent, if reading the response times out (there's no activity for the given period of time), a
    * failed effect will be returned, or an exception will be thrown
    */
  def readTimeout(t: Duration): PReq[T, R] =
    copyWith(options = options.copy(readTimeout = t))

  /** Specifies the target type to which the response body should be read. Note that this replaces any previous
    * specifications, which also includes any previous `mapResponse` invocations.
    */
  def response[T2, R2](ra: ResponseAs[T2, R2]): PReq[T2, R with R2] =
    copyWith(response = ra)

  def mapResponse[T2](f: T => T2): PReq[T2, R] =
    copyWith(response = response.map(f))

  def isWebSocket: Boolean = ResponseAs.isWebSocket(response)

  def followRedirects(fr: Boolean): PReq[T, R] =
    copyWith(options = options.copy(followRedirects = fr))

  def maxRedirects(n: Int): PReq[T, R] =
    if (n <= 0)
      copyWith(options = options.copy(followRedirects = false))
    else
      copyWith(options = options.copy(followRedirects = true, maxRedirects = n))

  def tag(k: String, v: Any): PReq[T, R] =
    copyWith(tags = tags + (k -> v))

  def tag(k: String): Option[Any] = tags.get(k)

  private val disableAutoDecompressionKey = "disableAutoDecompression"

  // Used as a workaround to keep binary compatibility
  // TODO: replace with additional parameter in RequestOptions when writing sttp4
  def disableAutoDecompression: PReq[T, R] = tag(disableAutoDecompressionKey, true)

  def autoDecompressionDisabled: Boolean = tags.getOrElse(disableAutoDecompressionKey, false).asInstanceOf[Boolean]

  private val loggingOptionsTagKey = "loggingOptions"

  /** Will only have effect when using the `LoggingBackend` */
  def logSettings(
      logRequestBody: Option[Boolean] = None,
      logResponseBody: Option[Boolean] = None,
      logRequestHeaders: Option[Boolean] = None,
      logResponseHeaders: Option[Boolean] = None
  ): PReq[T, R] = {
    val loggingOptions = LoggingOptions(
      logRequestBody = logRequestBody,
      logResponseBody = logResponseBody,
      logRequestHeaders = logRequestHeaders,
      logResponseHeaders = logResponseHeaders
    )
    this.tag(loggingOptionsTagKey, loggingOptions)
  }

  def loggingOptions: Option[LoggingOptions] = tag(loggingOptionsTagKey).asInstanceOf[Option[LoggingOptions]]

  /** When a POST or PUT request is redirected, should the redirect be a POST/PUT as well (with the original body), or
    * should the request be converted to a GET without a body.
    *
    * Note that this only affects 301 and 302 redirects. 303 redirects are always converted, while 307 and 308 redirects
    * always keep the same method.
    *
    * See https://developer.mozilla.org/en-US/docs/Web/HTTP/Redirections for details.
    */
  def redirectToGet(r: Boolean): PReq[T, R] =
    copyWith(options = options.copy(redirectToGet = r))

  def showBasic: String =
    (methodOpt, uriOpt) match {
      case (Some(m), Some(u)) =>
        val ws = if (isWebSocket) " (web socket) " else ""
        s"$m$ws $u"
      case _ => "(no method & uri set)"
    }

  def show(
      includeBody: Boolean = true,
      includeHeaders: Boolean = true,
      sensitiveHeaders: Set[String] = HeaderNames.SensitiveHeaders
  ): String = {
    val headers =
      if (includeHeaders) ", headers: " + this.headers.map(_.toStringSafe(sensitiveHeaders)).mkString(", ") else ""
    val body = if (includeBody) s", body: ${this.body.show}" else ""
    val methodAndUri = (methodOpt, uriOpt) match {
      case (Some(m), Some(uri)) =>
        val ws = if (isWebSocket) " (web socket) " else ""
        s"$m$ws $uri, "
      case _ => ""
    }
    s"${methodAndUri}response as: ${response.show}$headers$body"
  }

  private def hasContentType: Boolean = headers.exists(_.is(HeaderNames.ContentType))
  private def setContentTypeIfMissing(mt: MediaType): PReq[T, R] =
    if (hasContentType) this else contentType(mt)

  private[client3] def withBody[R2](body: RequestBody[R2]): PReq[T, R with R2] = {
    val defaultCt = body match {
      case StringBody(_, encoding, ct) =>
        ct.copy(charset = Some(encoding))
      case _ =>
        body.defaultContentType
    }

    setContentTypeIfMissing(defaultCt).copyWith(body = body)
  }

  private def hasContentLength: Boolean =
    headers.exists(_.name.equalsIgnoreCase(HeaderNames.ContentLength))
  private def setContentLengthIfMissing(l: => Long): PReq[T, R] =
    if (hasContentLength) this else contentLength(l)

  private def formDataBody(fs: Seq[(String, String)], encoding: String): PReq[T, R] = {
    val b = RequestBody.paramsToStringBody(fs, encoding)
    setContentTypeIfMissing(MediaType.ApplicationXWwwFormUrlencoded)
      .setContentLengthIfMissing(b.s.getBytes(encoding).length.toLong)
      .copyWith(body = b)
  }
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
final case class PartialRequest[T, -R](
    body: RequestBody[R],
    headers: Seq[Header],
    response: ResponseAs[T, R],
    options: RequestOptions,
    tags: Map[String, Any]
) extends HasHeaders
    with PartialRequestOps[T, R, PartialRequest]
    with RequestOps[Request[T, R]] {
  override def methodOpt: Option[Method] = None
  override def uriOpt: Option[Uri] = None
  override def copyWith[T2, R2](
      body: RequestBody[R2] = body,
      headers: Seq[Header] = headers,
      response: ResponseAs[T2, R2] = response,
      options: RequestOptions = options,
      tags: Map[String, Any] = tags
  ): PartialRequest[T2, R2] =
    PartialRequest(body, headers, response, options, tags)

  def toRequest(uri: Uri, method: Method): Request[T, R] =
    Request(method, uri, body, headers, response, options, tags)
}

class SpecifyAuthScheme[T, -R, PReq[X, -Y] <: PartialRequestOps[X, Y, PReq]](
    hn: String,
    rt: PReq[T, R],
    digestTag: String
) {
  def basic(user: String, password: String): PReq[T, R] = {
    val c = new String(Base64.getEncoder.encode(s"$user:$password".getBytes(Utf8)), Utf8)
    rt.header(hn, s"Basic $c")
  }

  def basicToken(token: String): PReq[T, R] =
    rt.header(hn, s"Basic $token")

  def bearer(token: String): PReq[T, R] =
    rt.header(hn, s"Bearer $token")

  def digest(user: String, password: String): PReq[T, R] = {
    rt.tag(digestTag, DigestAuthenticator.DigestAuthData(user, password))
  }
}

case class RequestOptions(
    followRedirects: Boolean,
    readTimeout: Duration,
    maxRedirects: Int,
    redirectToGet: Boolean
)
