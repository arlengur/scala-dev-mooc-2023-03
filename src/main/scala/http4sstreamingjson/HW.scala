package http4sstreamingjson

import cats.effect.kernel.Resource
import cats.effect.{IO, IOApp, Ref}
import com.comcast.ip4s.{Host, Port}
import fs2.Stream
import io.circe.derivation.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.{HttpRoutes, Request, Response, Uri}

import scala.concurrent.duration._

case class Counter(counter: Int)

object HW extends IOApp.Simple {
  type Sessions[F[_]] = Ref[F, Counter]

  implicit val encoderCounter: Encoder[Counter] = deriveEncoder
  implicit val decoderCounter: Decoder[Counter] = deriveDecoder

  def serviceOne(sessions: Sessions[IO]): HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "counter" =>
        sessions.getAndUpdate(cur => cur.copy(counter = cur.counter + 1)).flatMap { counter =>
          Ok(counter)
        }
      case GET -> Root / "slow" / IntVar(chunk) / IntVar(total) / IntVar(time) =>
        Ok(chunkFirst(chunk, total, time))
    }

  def serviceTwo: HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "slow" / IntVar(chunk) / IntVar(total) / IntVar(time) =>
        Ok(chunkFirst(chunk, total, time))
    }

  def chunkFirst(chunk: Int, total: Int, time: Int) = {
    Stream(5).repeat.delayBy[IO](time.seconds).map(x => x.toByte)
      .chunkN(chunk).take(total).attempt.compile.drain
  }

  def router(sessions: Sessions[IO]) = Router("/" -> serviceOne(sessions))

  val server = for {
    sessions <- Resource.eval(Ref.of[IO, Counter](Counter(0)))
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(router(sessions).orNotFound).build
  } yield s

  def run(): IO[Unit] = {
    server.use(_ => IO.never)
  }
}

object HttpClient {
  val builder: Resource[IO, Client[IO]] = EmberClientBuilder.default[IO].build
  val request: Request[IO] = Request[IO](uri = Uri.fromString("http://localhost:8080/slow/10/1024/5").toOption.get)

  val result: Resource[IO, Response[IO]] = for {
    client <- builder
    response <- client.run(request)
  } yield response
}

object main extends IOApp.Simple {
  def run(): IO[Unit] = for {
    fiber <- HW.server.use(_ => IO.never).start
    _ <- HttpClient.result.use(IO.println)
    _ <- fiber.join
  } yield ()
}

