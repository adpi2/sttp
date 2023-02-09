package sttp.client3.logging

import sttp.client3._
import sttp.client3.listener.ListenerBackend

object LoggingBackend {
  def apply[F[_]](delegate: Backend[F], logger: Logger[F]): delegate.SelfType =
    apply(delegate, logger, LogConfig.Default)

  def apply[F[_]](delegate: Backend[F], logger: Logger[F], config: LogConfig): delegate.SelfType =
    apply(delegate, Log.default(logger, config), config.includeTiming, config.logResponseBody)

  def apply[F[_]](
      delegate: Backend[F],
      log: Log[F],
      includeTiming: Boolean,
      logResponseBody: Boolean
  ): delegate.SelfType =
    if (logResponseBody) LoggingWithResponseBodyBackend(delegate, log, includeTiming)
    else ListenerBackend(delegate, new LoggingListener(log, includeTiming)(delegate.genericBackend.responseMonad))
}
