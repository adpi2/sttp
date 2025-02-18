package sttp.client3.httpclient.cats

import cats.effect.IO
import sttp.client3.impl.cats.CatsRetryTest
import sttp.client3.testing.HttpTest

class HttpClientCatsHttpTest extends HttpTest[IO] with CatsRetryTest with HttpClientCatsTestBase {
  override def supportsHostHeaderOverride = false

  override def supportsDeflateWrapperChecking = false
}
