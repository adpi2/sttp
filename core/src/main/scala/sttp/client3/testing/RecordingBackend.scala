package sttp.client3.testing

import java.util.concurrent.atomic.AtomicReference
import java.util.function.UnaryOperator
import sttp.client3._
import sttp.monad.syntax._

import scala.util.{Failure, Success, Try}
import sttp.capabilities.Effect
import sttp.client3.testing.RecordingBackend.RequestAndResponse

trait RecordingBackend {
  def allInteractions: List[RequestAndResponse]
}

class GenericRecordingBackend[F[_], P](delegate: GenericBackend[F, P])
    extends DelegateSttpBackend[F, P](delegate)
    with RecordingBackend {

  private val _allInteractions = new AtomicReference[Vector[RequestAndResponse]](Vector())

  private def addInteraction(request: AbstractRequest[_, _], response: Try[Response[_]]): Unit = {
    _allInteractions.updateAndGet(new UnaryOperator[Vector[RequestAndResponse]] {
      override def apply(t: Vector[RequestAndResponse]): Vector[RequestAndResponse] = t.:+((request, response))
    })
  }

  override def send[T](request: AbstractRequest[T, P with Effect[F]]): F[Response[T]] = {
    delegate
      .send(request)
      .map { response =>
        addInteraction(request, Success(response))
        response
      }
      .handleError { case e: Exception =>
        addInteraction(request, Failure(e))
        responseMonad.error(e)
      }
  }

  override def allInteractions: List[RequestAndResponse] = _allInteractions.get().toList
}

object RecordingBackend {
  type RequestAndResponse = (AbstractRequest[_, _], Try[Response[_]])

  def apply[F[_]](delegate: Backend[F]): (delegate.SelfType, RecordingBackend) = {
    val recording = new GenericRecordingBackend(delegate.genericBackend)
    (delegate.wrap(_ => recording), recording)
  }
}
