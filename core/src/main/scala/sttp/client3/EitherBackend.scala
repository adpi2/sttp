package sttp.client3

import sttp.client3.monad.FunctionK
import sttp.monad.EitherMonad

/** A synchronous backend that safely wraps exceptions in `Either[Throwable, *]`'s */
object EitherBackend {
  def apply(backend: SyncBackend): Backend[Either[Throwable, *]] =
    Backend[Either[Throwable, *]](new MappedEffectBackend(backend.genericBackend, idToEither, eitherToId, EitherMonad))

  private val eitherToId: FunctionK[Either[Throwable, *], Identity] =
    new FunctionK[Either[Throwable, *], Identity] {
      override def apply[A](fa: Either[Throwable, A]): Identity[A] =
        fa match {
          case Left(e)  => throw e
          case Right(v) => v
        }
    }

  private val idToEither: FunctionK[Identity, Either[Throwable, *]] =
    new FunctionK[Identity, Either[Throwable, *]] {
      override def apply[A](fa: Identity[A]): Either[Throwable, A] = Right(fa)
    }
}
