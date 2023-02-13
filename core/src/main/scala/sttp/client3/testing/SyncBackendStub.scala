package sttp.client3.testing

import sttp.client3._
import sttp.client3.monad.IdMonad

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
class SyncBackendStub(
    matchers: PartialFunction[AbstractRequest[_, _], Response[_]],
    fallback: Option[GenericBackend[Identity, Any]]
) extends BackendStub[Identity, Any](IdMonad, matchers, fallback)
    with SyncBackend {

  override type SelfStubType = SyncBackendStub
  override type SelfType = SyncBackend

  override def wrap(f: GenericBackend[Identity, Any] => GenericBackend[Identity, Any]): SyncBackend = SyncBackend(
    f(this)
  )

  override protected def withMatchers(matchers: PartialFunction[AbstractRequest[_, _], Response[_]]) =
    new SyncBackendStub(matchers, fallback)
}

/** A stub of a synchronous backend. */
object SyncBackendStub extends SyncBackendStub(PartialFunction.empty, None) {

  /** Create a stub backend which delegates send requests to the given fallback backend, if the request doesn't match
    * any of the specified predicates.
    */
  def withFallback(fallback: SyncBackend): SyncBackendStub =
    new SyncBackendStub(PartialFunction.empty, Some(fallback.genericBackend))
}
