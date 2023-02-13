package sttp.client3

import sttp.client3.monad.FunctionK
import sttp.monad.TryMonad

import scala.util.Try

/** A synchronous backend that safely wraps exceptions in `Try`'s */
object TryBackend {
  def apply(backend: SyncBackend): Backend[Try] =
    Backend[Try](new MappedEffectBackend(backend.genericBackend, idToTry, tryToId, TryMonad))

  private val tryToId: FunctionK[Try, Identity] =
    new FunctionK[Try, Identity] {
      override def apply[A](fa: Try[A]): Identity[A] = fa.get
    }

  private val idToTry: FunctionK[Identity, Try] =
    new FunctionK[Identity, Try] {
      override def apply[A](fa: Identity[A]): Try[A] = Try(fa)
    }
}
