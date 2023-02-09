package sttp.client3.armeria.future

import com.linecorp.armeria.client.WebClient
import com.linecorp.armeria.common.HttpData
import com.linecorp.armeria.common.stream.StreamMessage
import org.reactivestreams.Publisher
import sttp.client3.armeria.ArmeriaWebClient.newClient
import sttp.client3.armeria.{GenericArmeriaBackend, BodyFromStreamMessage}
import sttp.client3.internal.NoStreams
import sttp.client3.{Backend, FollowRedirectsBackend, SttpBackendOptions}
import sttp.monad.{FutureMonad, MonadAsyncError}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

private final class ArmeriaFutureBackend(client: WebClient, closeFactory: Boolean)
    extends GenericArmeriaBackend[Future, Nothing](client, closeFactory, new FutureMonad()) {

  override val streams: NoStreams = NoStreams

  override protected def bodyFromStreamMessage: BodyFromStreamMessage[Future, Nothing] =
    new BodyFromStreamMessage[Future, Nothing] {

      override val streams: NoStreams = NoStreams

      override implicit def monad: MonadAsyncError[Future] = new FutureMonad()

      override def publisherToStream(streamMessage: StreamMessage[HttpData]): streams.BinaryStream =
        throw new UnsupportedOperationException("This backend does not support streaming")
    }

  override protected def streamToPublisher(stream: streams.BinaryStream): Publisher[HttpData] =
    throw new UnsupportedOperationException("This backend does not support streaming")
}

object ArmeriaFutureBackend {

  /** Creates a new Armeria backend, using the given or default `SttpBackendOptions`. Due to these customisations, the
    * client will manage its own connection pool. If you'd like to reuse the default Armeria
    * [[https://armeria.dev/docs/client-factory ClientFactory]] use `.usingDefaultClient`.
    */
  def apply(options: SttpBackendOptions = SttpBackendOptions.Default): Backend[Future] =
    apply(newClient(options), closeFactory = true)

  def usingClient(client: WebClient): Backend[Future] =
    apply(client, closeFactory = false)

  def usingDefaultClient(): Backend[Future] =
    apply(newClient(), closeFactory = false)

  private def apply(client: WebClient, closeFactory: Boolean): Backend[Future] =
    FollowRedirectsBackend(new ArmeriaFutureBackend(client, closeFactory))
}
