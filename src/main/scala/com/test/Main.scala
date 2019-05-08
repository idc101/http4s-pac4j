package com.test

import cats.effect._
import org.http4s._
import org.http4s.dsl.io._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import fs2.{Stream, StreamApp}
import fs2.StreamApp.ExitCode
import org.http4s.server.blaze._
import org.pac4j.http4s.{CallbackService, SecurityFilterMiddleware, Session, SessionConfig}

object Main extends StreamApp[IO] {

  val sessionConfig = SessionConfig(
    cookieName = "session",
    mkCookie = Cookie(_, _),
    secret = "This is a secret",
    maxAge = 5.minutes
  )

  val config = new DemoConfigFactory().build()
  val callbackService = new CallbackService(config)

  val root = HttpService[IO] {
    case GET -> Root =>
      Ok(s"Hello World")
  }

  val protectedPages = HttpService[IO] {
    case GET -> Root / "protected" =>
      Ok(s"Done")
  }

  val authedProtectedPages = SecurityFilterMiddleware.securityFilter(protectedPages, config)

  val loginCallBack: HttpService[IO] = HttpService[IO] {
    case req @ GET -> Root / "login" =>
      callbackService.login(req)
    case req @ POST -> Root / "login" =>
      callbackService.login(req)
  }
  //val newService = MyMiddle(helloWorldService, Header("SomeKey", "SomeValue"))

  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] = {
    BlazeBuilder[IO]
      .bindHttp(8080, "localhost")
      .globalMiddleware(Session.sessionManagement(config))
      .mountService(root, "/")
      .mountService(authedProtectedPages, "/")
      .mountService(loginCallBack, "/")
      .serve
  }
}

object MyMiddle {
  def addHeader(resp: Response[IO], header: Header) =
    resp match {
      case Status.Successful(resp) => resp.putHeaders(header)
      case resp => resp
    }

  def apply(service: HttpService[IO], header: Header) =
    service.map(addHeader(_, header))
}
