package endpoints.http4s.server

import endpoints.algebra.BasicAuthentication.Credentials
import endpoints.algebra.Documentation
import org.http4s
import org.http4s.{BasicCredentials, Status}
import org.http4s.headers.Authorization

trait BasicAuthentication[F[_]]
    extends Endpoints[F]
    with endpoints.algebra.BasicAuthentication {

  private[endpoints] def basicAuthenticationHeader
    : RequestHeaders[Credentials] =
    headers =>
      headers
        .get(Authorization)
        .map { authHeader =>
          authHeader.credentials match {
            case BasicCredentials(username, password) =>
              Right(Credentials(username, password))
            case _ => Left(badRequestResponse)
          }
        }
        .getOrElse(Left(badRequestResponse))

  private[endpoints] def authenticated[A](
      response: Response[A],
      docs: Documentation): Response[Option[A]] = {
    case Some(a) => response(a)
    case None =>
      http4s.Response[F](status = Status.Forbidden)
  }
}
