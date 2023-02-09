package sttp.client3.testing

import sttp.client3._
import sttp.client3.monad.IdMonad
import sttp.monad.{FutureMonad, MonadError}

import scala.concurrent.{ExecutionContext, Future}

/** A stub backend to use in tests.
  *
  * The stub can be configured to respond with a given response if the request matches a predicate (see the
  * [[whenRequestMatches()]] method).
  *
  * Note however, that this is not type-safe with respect to the type of the response body - the stub doesn't have a way
  * to check if the type of the body in the configured response is the same as the one specified by the request. Some
  * conversions will be attempted (e.g. from a `String` to a custom mapped type, as specified in the request, see the
  * documentation for more details).
  *
  * Predicates can match requests basing on the URI or headers. A [[ClassCastException]] might occur if for a given
  * request, a response is specified with the incorrect or inconvertible body type.
  */
class EffectBackendStub[F[_]](
    monad: MonadError[F],
    matchers: PartialFunction[AbstractRequest[_, _], F[Response[_]]],
    fallback: Option[GenericBackend[F, Any]]
) extends BackendStub[F, Any](monad, matchers, fallback)
    with EffectBackend[F] {

  override type SelfStubType = EffectBackendStub[F]

  override protected def withMatchers(
      matchers: PartialFunction[AbstractRequest[_, _], F[Response[_]]]
  ): EffectBackendStub[F] =
    new EffectBackendStub(monad, matchers, fallback)
}

object EffectBackendStub {

  /** Create a stub of a synchronous backend (which doesn't use an effect type) */
  def synchronous: EffectBackendStub[Identity] = new EffectBackendStub(IdMonad, PartialFunction.empty, None)

  /** Create a stub of an asynchronous backend (which uses the Scala's built-in [[Future]] as the effect type). */
  def asynchronousFuture(implicit ec: ExecutionContext): EffectBackendStub[Future] =
    new EffectBackendStub(new FutureMonad(), PartialFunction.empty, None)

  /** Create a stub backend using the given response monad (which determines the effect type for responses). */
  def apply[F[_]](monad: MonadError[F]): EffectBackendStub[F] =
    new EffectBackendStub(monad, PartialFunction.empty, None)

  /** Create a stub backend which delegates send requests to the given fallback backend, if the request doesn't match
    * any of the specified predicates.
    */
  def withFallback[F[_]](fallback: EffectBackend[F]): EffectBackendStub[F] =
    new EffectBackendStub[F](
      fallback.genericBackend.responseMonad,
      PartialFunction.empty,
      Some(fallback.genericBackend)
    )
}
