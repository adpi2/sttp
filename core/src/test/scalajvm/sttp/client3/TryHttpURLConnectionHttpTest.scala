package sttp.client4

import sttp.client4.testing.{ConvertToFuture, HttpTest}

import scala.util.Try

class TryHttpURLConnectionHttpTest extends HttpTest[Try] {

  override val backend: Backend[Try] = TryHttpURLConnectionBackend()
  override implicit val convertToFuture: ConvertToFuture[Try] = ConvertToFuture.scalaTry

  override def supportsCancellation = false
  override def supportsHostHeaderOverride = false
  override def supportsDeflateWrapperChecking = false

  override def timeoutToNone[T](t: Try[T], timeoutMillis: Int): Try[Option[T]] = t.map(Some(_))
}
