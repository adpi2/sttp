package sttp.client4.examples

import io.circe.generic.auto._
import sttp.client4._
import sttp.client4.circe._
import sttp.client4.httpclient.zio.HttpClientZioBackend
import zio._

object GetAndParseJsonZioCirce extends ZIOAppDefault {

  override def run = {

    case class HttpBinResponse(origin: String, headers: Map[String, String])

    val request = basicRequest
      .get(uri"https://httpbin.org/get")
      .response(asJson[HttpBinResponse])

    // create a description of a program, which requires SttpClient dependency in the environment
    def sendAndPrint(backend: Backend[Task]): Task[Unit] = for {
      response <- request.send(backend)
      _ <- Console.printLine(s"Got response code: ${response.code}")
      _ <- Console.printLine(response.body.toString)
    } yield ()

    // provide an implementation for the SttpClient dependency
    HttpClientZioBackend.scoped().flatMap(sendAndPrint)
  }
}
