package org.pac4j.http4s

import cats.effect.IO
import org.http4s.dsl.io.{->, /, GET, Ok, POST, Root}
import org.http4s.{Header, HttpService, Response, Status}
import org.pac4j.core.config.Config
import org.pac4j.core.engine.DefaultCallbackLogic
import org.pac4j.core.http.adapter.HttpActionAdapter
import cats.effect._, org.http4s._, org.http4s.dsl.io._


/**
  * Http4s Service to handle callback from Id Provider
  */
class CallbackService(config: Config) {
  val defaultUrl: String = null

  val saveInSession: Boolean = false

  val multiProfile: Boolean = false

  val renewSession: Boolean = false

  val defaultClient: String = null

  def login(request: Request[IO]): IO[Response[IO]] = {
    val callbackLogic = new DefaultCallbackLogic[Response[IO], Http4sWebContext]()
    Ok().map { resp =>
      val webContext = new Http4sWebContext(request, resp)
      callbackLogic.perform(webContext,
        config,
        config.getHttpActionAdapter.asInstanceOf[HttpActionAdapter[Response[IO], Http4sWebContext]],
        this.defaultUrl,
        this.saveInSession,
        this.multiProfile,
        this.renewSession,
        this.defaultClient)
    }
  }
}
