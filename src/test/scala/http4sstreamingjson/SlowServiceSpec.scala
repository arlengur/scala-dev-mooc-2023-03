package http4sstreamingjson

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{IO, Ref}
import org.http4s.implicits._
import org.http4s.{Method, Request, Uri}
import org.scalatest.freespec.AsyncFreeSpec

class SlowServiceSpec extends AsyncFreeSpec with AsyncIOSpec {
  type Sessions[F[_]] = Ref[F, Counter]

  "should return a chunk of data with delay" in {
    val request = Request[IO](Method.GET, Uri.fromString("/slow/5/10/2").toOption.get)

    HW.serviceTwo.orNotFound(request).flatMap { response =>
      response.as[String].map { body =>
        assert(response.status.code == 200)
//        assert(body.getBytes.length == 10)
      }
    }
  }
}