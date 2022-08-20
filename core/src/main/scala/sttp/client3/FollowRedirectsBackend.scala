package sttp.client3

import sttp.capabilities.Effect
import sttp.model.{Method, StatusCode, _}

/** @param transformUri
  *   Defines if and how [[Uri]] s from the `Location` header should be transformed. For example, this enables changing
  *   the encoding of host, path, query and fragment segments to be more strict or relaxed.
  */
class FollowRedirectsBackend[F[_], P](
    delegate: SttpBackend[F, P],
    contentHeaders: Set[String] = HeaderNames.ContentHeaders,
    sensitiveHeaders: Set[String] = HeaderNames.SensitiveHeaders,
    transformUri: Uri => Uri = FollowRedirectsBackend.DefaultUriTransform
) extends DelegateSttpBackend[F, P](delegate) {

  // this is needed to maintain binary compatibility with 3.3.14 and earlier
  def this(delegate: SttpBackend[F, P], contentHeaders: Set[String], sensitiveHeaders: Set[String]) =
    this(delegate, contentHeaders, sensitiveHeaders, FollowRedirectsBackend.DefaultUriTransform)

  type PE = P with Effect[F]

  override def send[T, R >: PE](request: Request[T, R]): F[Response[T]] = {
    sendWithCounter(request, 0)
  }

  private def sendWithCounter[T, R >: PE](request: Request[T, R], redirects: Int): F[Response[T]] = {
    // if there are nested follow redirect backends, disabling them and handling redirects here
    val resp = delegate.send(request.followRedirects(false))
    if (request.options.followRedirects) {
      responseMonad.flatMap(resp) { (response: Response[T]) =>
        if (response.isRedirect) {
          followRedirect(request, response, redirects)
        } else {
          responseMonad.unit(response)
        }
      }
    } else {
      resp
    }
  }

  private def followRedirect[T, R >: PE](
      request: Request[T, R],
      response: Response[T],
      redirects: Int
  ): F[Response[T]] = {
    response.header(HeaderNames.Location).fold(responseMonad.unit(response)) { loc =>
      if (redirects >= request.options.maxRedirects) {
        responseMonad.error(TooManyRedirectsException(request.uri, redirects))
      } else {
        followRedirect(request, response, redirects, loc)
      }
    }
  }

  private def followRedirect[T, R >: PE](
      request: Request[T, R],
      response: Response[T],
      redirects: Int,
      loc: String
  ): F[Response[T]] = {
    val uri = if (FollowRedirectsBackend.isRelative(loc)) {
      transformUri(request.uri.resolve(uri"$loc"))
    } else {
      transformUri(uri"$loc")
    }

    val redirectResponse =
      ((stripSensitiveHeaders[T, R](_)) andThen
        (changePostPutToGet[T, R](_, response.code)) andThen
        (sendWithCounter(_, redirects + 1)))
        .apply(request.copy[T, R](uri = uri))

    responseMonad.map(redirectResponse) { rr =>
      val responseNoBody = response.copy(body = ())
      rr.copy(history = responseNoBody :: rr.history)
    }
  }

  private def stripSensitiveHeaders[T, R](request: Request[T, R]): Request[T, R] = {
    request.copy[T, R](
      headers = request.headers.filterNot(h => sensitiveHeaders.contains(h.name.toLowerCase()))
    )
  }

  private def changePostPutToGet[T, R](r: Request[T, R], statusCode: StatusCode): Request[T, R] = {
    val applicable = r.method == Method.POST || r.method == Method.PUT
    val alwaysChanged = statusCode == StatusCode.SeeOther
    val neverChanged = statusCode == StatusCode.TemporaryRedirect || statusCode == StatusCode.PermanentRedirect
    if (applicable && (r.options.redirectToGet || alwaysChanged) && !neverChanged) {
      // when transforming POST or PUT into a get, content is dropped, also filter out content-related request headers
      r.method(Method.GET, r.uri)
        .copy(
          body = NoBody,
          headers = r.headers.filterNot(header => contentHeaders.contains(header.name.toLowerCase()))
        )
    } else r
  }
}

object FollowRedirectsBackend {
  private[client3] val MaxRedirects = 32

  private val protocol = "^[a-z]+://.*".r

  private[client3] def isRelative(uri: String): Boolean = {
    val toCheck = uri.toLowerCase().trim
    !protocol.pattern.matcher(toCheck).matches()
  }

  /** By default, the conversion is a no-op */
  val DefaultUriTransform: Uri => Uri = (uri: Uri) => uri
}

case class TooManyRedirectsException(uri: Uri, redirects: Int) extends Exception
