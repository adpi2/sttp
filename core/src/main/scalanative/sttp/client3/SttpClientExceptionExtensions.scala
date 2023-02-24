package sttp.client4

import sttp.client4.SttpClientException.{ConnectException, ReadException, TimeoutException}
import sttp.client4.ws.{GotAWebSocketException, NotAWebSocketException}

import scala.annotation.tailrec

trait SttpClientExceptionExtensions {
  @tailrec
  final def defaultExceptionToSttpClientException(request: GenericRequest[_, _], e: Exception): Option[Exception] =
    e match {
      case e: java.net.ConnectException             => Some(new ConnectException(request, e))
      case e: java.net.UnknownHostException         => Some(new ConnectException(request, e))
      case e: java.net.MalformedURLException        => Some(new ConnectException(request, e))
      case e: java.net.NoRouteToHostException       => Some(new ConnectException(request, e))
      case e: java.net.PortUnreachableException     => Some(new ConnectException(request, e))
      case e: java.net.ProtocolException            => Some(new ConnectException(request, e))
      case e: java.net.URISyntaxException           => Some(new ConnectException(request, e))
      case e: java.util.concurrent.TimeoutException => Some(new TimeoutException(request, e))
      case e: java.net.SocketTimeoutException       => Some(new TimeoutException(request, e))
      case e: java.net.UnknownServiceException      => Some(new ReadException(request, e))
      case e: java.net.SocketException              => Some(new ReadException(request, e))
      case e: java.io.IOException                   => Some(new ReadException(request, e))
      case e: NotAWebSocketException                => Some(new ReadException(request, e))
      case e: GotAWebSocketException                => Some(new ReadException(request, e))
      case e: ResponseException[_, _]               => Some(new ReadException(request, e))
      case e if e.getCause != null && e.getCause.isInstanceOf[Exception] =>
        defaultExceptionToSttpClientException(request, e.getCause.asInstanceOf[Exception])
      case _ => None
    }
}
