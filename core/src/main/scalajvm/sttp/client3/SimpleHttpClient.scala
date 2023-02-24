package sttp.client4

/** A simple, synchronous http client. Usage example:
  *
  * {{{
  * import sttp.client4.{SimpleHttpClient, UriContext, basicRequest}
  *
  * val client = SimpleHttpClient()
  * val request = basicRequest.get(uri"https://httpbin.org/get")
  * val response = client.send(request)
  * println(response.body)
  * }}}
  *
  * Wraps a [[SyncBackend]], which can be substituted or modified using [[wrapBackend]], adding e.g. logging.
  *
  * Creating a client allocates resources, hence when no longer needed, the client should be closed using [[close]].
  */
case class SimpleHttpClient(backend: SyncBackend) {

  def send[T](request: Request[T]): Response[T] = backend.send(request)

  def withBackend(newBackend: SyncBackend): SimpleHttpClient = copy(backend = newBackend)
  def wrapBackend(f: SyncBackend => SyncBackend): SimpleHttpClient = copy(backend = f(backend))

  def close(): Unit = backend.close()
}

object SimpleHttpClient {
  def apply(): SimpleHttpClient = SimpleHttpClient(HttpClientSyncBackend())

  /** Runs the given function `f` with a new, default instance of [[SimpleHttpClient]] and closes the client after the
    * function completes, cleaning up any resources.
    */
  def apply[T](f: SimpleHttpClient => T): T = {
    val client = SimpleHttpClient()
    try f(client)
    finally client.close()
  }
}
