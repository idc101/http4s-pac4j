package org.pac4j.http4s

import io.circe.jawn
import java.util.Date

import scala.util.{Failure, Success, Try}
import javax.crypto.{Cipher, Mac}
import javax.crypto.spec.SecretKeySpec
import javax.xml.bind.DatatypeConverter
import org.http4s.{AttributeKey, Cookie, HttpService, Request, Response}
import org.http4s.Http4s._
import org.http4s.headers.{Cookie => CookieHeader}
import org.http4s.server.{HttpMiddleware, Middleware}

import scala.concurrent.duration.Duration
import scala.util.Try
import cats.data.{Kleisli, OptionT}
import org.pac4j.core.config.Config
import org.pac4j.http4s.session.Session
//import scalaz.Scalaz._
import cats.effect._

object Syntax {
  implicit final class RequestOps(val v: Request[IO]) extends AnyVal {
    def session: IO[Option[Session]] =
      IO.pure(v.attributes.get(Session.requestAttr))
  }

  implicit final class TaskResponseOps(val v: IO[Response[IO]]) extends AnyVal {
    def clearSession: IO[Response[IO]] =
      v.withAttribute(Session.responseAttr(_ => None))

    def modifySession(f: Session => Session): IO[Response[IO]] = {
      val lf: Option[Session] => Option[Session] = _.fold[Option[Session]](None)(f.andThen(Some(_)))
      v.map { response =>
        response.withAttribute(Session.responseAttr(response.attributes.get(Session.responseAttr).fold(lf)(_.andThen(lf))))
      }
    }

    def newSession(session: Session): IO[Response[IO]] =
      v.withAttribute(Session.responseAttr(_ => Some(session)))
  }
}

final case class SessionConfig(
  cookieName: String,
  mkCookie: (String, String) => Cookie,
  secret: String,
  maxAge: Duration
) {
  // TODO: Type for this
  require(secret.length >= 16)

  def constantTimeEquals(a: String, b: String): Boolean =
    if (a.length != b.length) {
      false
    } else {
      var equal = 0
      for (i <- Array.range(0, a.length)) {
        equal |= a(i) ^ b(i)
      }
      equal == 0
    }

  private[this] def keySpec: SecretKeySpec =
    new SecretKeySpec(secret.substring(0, 16).getBytes("UTF-8"), "AES")

  private[this] def encrypt(content: String): String = {
    // akka-http-session pads content to guarantee it's non-empty
    // we require maxAge so it can never be empty.
    val cipher = Cipher.getInstance("AES")
    cipher.init(Cipher.ENCRYPT_MODE, keySpec)
    DatatypeConverter.printHexBinary(cipher.doFinal(content.getBytes("UTF-8")))
  }

  private[this] def decrypt(content: String): Option[String] = {
    val cipher = Cipher.getInstance("AES")
    cipher.init(Cipher.DECRYPT_MODE, keySpec)
    Try(new String(cipher.doFinal(DatatypeConverter.parseHexBinary(content)), "UTF-8")).toOption
  }

  private[this] def sign(content: String): String = {
    val signKey = secret.getBytes("UTF-8")
    val signMac = Mac.getInstance("HmacSHA1")
    signMac.init(new SecretKeySpec(signKey, "HmacSHA256"))
    DatatypeConverter.printBase64Binary(signMac.doFinal(content.getBytes("UTF-8")))
  }

  def cookie(content: String): IO[Cookie] =
    IO {
      val now = new Date().getTime / 1000
      val expires = now + maxAge.toSeconds
      val serialized = s"$expires-$content"
      val signed = sign(serialized)
      val encrypted = encrypt(serialized)
      mkCookie(cookieName, s"$signed-$encrypted")
    }

  def check(cookie: Cookie): IO[Option[String]] =
    IO {
      val now = new Date().getTime / 1000
      cookie.content.split('-') match {
        case Array(signature, value) =>
          for {
            decrypted <- decrypt(value)
            if constantTimeEquals(signature, sign(decrypted))
            Array(expires, content) = decrypted.split("-", 2)
            expiresSeconds <- Try(expires.toLong).toOption
            if expiresSeconds > now
          } yield content
        case _ =>
          None
      }
    }
}

object Session {
  val requestAttr = AttributeKey[Session] //("com.github.hgiddens.http4s.session.Session")
  val responseAttr = AttributeKey[Option[Session] => Option[Session]] //("com.github.hgiddens.http4s.session.Session")

  private[this] def sessionAsCookie(config: SessionConfig, session: Session): IO[Cookie] =
    config.cookie(session.noSpaces)

  private[this] def checkSignature(config: SessionConfig, cookie: Cookie): IO[Option[Session]] =
    config.check(cookie).map(_.flatMap(jawn.parse(_).toOption))

  private[this] def sessionFromRequest(config: SessionConfig, request: Request[IO]): IO[Option[Session]] = {
    (for {
      allCookies <- OptionT(IO.pure(CookieHeader.from(request.headers)))
      sessionCookie <- OptionT(IO.pure(allCookies.values.toList.find(_.name == config.cookieName)))
      session <- OptionT(checkSignature(config, sessionCookie))
    } yield session).value
  }

  def sessionManagement(service: HttpService[IO], config: Config): HttpService[IO] = Kleisli { request: Request[IO] =>
    service(request).map { response =>
      for {
        requestSession <- sessionFromRequest(config, request)
        requestWithSession = requestSession.fold(request)(session => request.withAttribute(requestAttr, session))
        response <- service(requestWithSession)
        updateSession <- response.attributes.get(responseAttr) // | identity
        responseWithSession <- updateSession(requestSession).fold(IO.pure(if (requestSession.isDefined) response.removeCookie(config.cookieName) else response))(session => sessionAsCookie(config, session).map(response.addCookie))
      } yield responseWithSession
    }
  }

//  def sessionManagement(config: SessionConfig): HttpMiddleware[IO] =
//  Middleware { (request, service) =>
//    for {
//      requestSession <- sessionFromRequest(config, request)
//      requestWithSession = requestSession.fold(request)(session => request.withAttribute(requestAttr, session))
//      response <- service(requestWithSession)
//      updateSession <- response.attributes.get(responseAttr) // | identity
//      responseWithSession <- updateSession(requestSession).fold(IO.pure(if (requestSession.isDefined) response.removeCookie(config.cookieName) else response))(session => sessionAsCookie(config, session).map(response.addCookie))
//    } yield responseWithSession
//  }

//  def sessionRequired(fallback: IO[Response[IO]]): HttpMiddleware[IO] =
//    Middleware { (request, service) =>
//      import Syntax._
//      OptionT(request.session).flatMapF(_ => service(request).value).getOrElseF(fallback)
//    }

//  def sessionRequired(service: HttpService[IO], fallback: IO[Response[IO]]): HttpMiddleware[IO] = cats.data.Kleisli { request: Request[IO] =>
//    import Syntax._
//    val x = service(request)
//    x
//    OptionT(request.session).flatMapF(_ => service(request)).getOrElseF(fallback)
//  }
//    Middleware { (request, service) =>
//      import Syntax._
//      OptionT(request.session).flatMapF(_ => service(request).value).getOrElseF(fallback)
//    }
}
