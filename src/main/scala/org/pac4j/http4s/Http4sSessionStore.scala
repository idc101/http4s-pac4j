package org.pac4j.http4s

import org.pac4j.core.context.session.SessionStore
import org.pac4j.http4s.Syntax._

object Http4sSessionStore extends SessionStore[Http4sWebContext] {
  override def getOrCreateSessionId(context: Http4sWebContext): String = ???

  override def get(context: Http4sWebContext, key: String): AnyRef = {
    val resultIO = context.request.session.map { sessionOpt =>
      for {
        session <- sessionOpt
        obj <- session.asObject
        value <- obj(key)
      } yield value
    }
    resultIO.unsafeRunSync() match {
      case Some(s) => s
      case None => null
    }
  }

  override def set(context: Http4sWebContext, key: String, value: Any): Unit = {
    val resultIO = context.request.session.map { sessionOpt =>
      println(sessionOpt)
      for {
        session <- sessionOpt
        obj <- session.asObject
        newSession <- obj.add(key, Json.)
        value <- obj(key)
      } yield value
    }
    resultIO.unsafeRunSync() match {
      case Some(s) => s
      case None => null
    }
  }

  override def destroySession(context: Http4sWebContext): Boolean = ???

  override def getTrackableSession(context: Http4sWebContext): AnyRef = ???

  override def buildFromTrackableSession(context: Http4sWebContext, trackableSession: Any): SessionStore[Http4sWebContext] = ???

  override def renewSession(context: Http4sWebContext): Boolean = ???
}
