package org.pac4j.http4s

import java.util
import java.util.Collection

import cats.data.Kleisli
import cats.effect.IO
import org.http4s.{HttpService, Request, Status}
import org.pac4j.core.config.Config
import org.pac4j.core.engine.{DefaultSecurityLogic, SecurityGrantedAccessAdapter, SecurityLogic}
import org.pac4j.core.http.adapter.HttpActionAdapter
import org.pac4j.core.profile.CommonProfile
import org.pac4j.core.util.CommonHelper.assertNotNull

object SecurityFilterMiddleware {
  val SECURITY_GRANTED_ACCESS = "SECURITY_GRANTED_ACCESS"

  private val clients: String = null

  private val authorizers: String = null

  private val matchers: String = null

  private val multiProfile: Boolean = false

  def securityFilter(service: HttpService[IO], config: Config): HttpService[IO] = Kleisli { request: Request[IO] =>
    service(request).map { response =>
      val securityLogic = new DefaultSecurityLogic[String, Http4sWebContext]
      val context = new Http4sWebContext(request, response) //, config.getSessionStore)

      val securityGrantedAccessAdapter = new SecurityGrantedAccessAdapter[String, Http4sWebContext] {
        override def adapt(context: Http4sWebContext, profiles: util.Collection[CommonProfile], parameters: AnyRef*): String = SECURITY_GRANTED_ACCESS
      }
      val result = securityLogic.perform(context, config, securityGrantedAccessAdapter,
        config.getHttpActionAdapter.asInstanceOf[HttpActionAdapter[String, Http4sWebContext]],
        this.clients, this.authorizers, this.matchers, this.multiProfile)

      if (result eq SECURITY_GRANTED_ACCESS) { // It means that the access is granted: continue
        println("Received SECURITY_GRANTED_ACCESS -> continue")
        response
      } else {
        println("Halt the request processing")
        // stop the processing if no SECURITY_GRANTED_ACCESS has been received
        response.withStatus(Status.Unauthorized)
      }
    }
  }
}
