package projection

import akka.Done
import akka.projection.eventsourced.EventEnvelope
import akka.projection.scaladsl.Handler
import application.UserEntityRepository
import domain.UserEntity

import scala.concurrent.Future

class UserProjectionHandler(userRepository: UserEntityRepository) extends Handler[EventEnvelope[UserEntity.Event]]() {

  override def process(envelope: EventEnvelope[UserEntity.Event]): Future[Done] =
    envelope.event match {
      case UserEntity.RegisteredUserEvent(id, firstName, lastName, userId, email, passwordHash, createdAt, verificationTokenHash, tokenExpirationDate, loginAttempts) => ???
      case UserEntity.VerificationEmailRequested(id, email, token) => ???
      case UserEntity.UserVerifiedEvent(id, verifiedAt) => ???
      case UserEntity.UserLockedEvent(id, lockedAt) => ???
      case UserEntity.UserLoggedInEvent(id, loggedInAt) => ???
      case UserEntity.UserUnlockedEvent(id, newPassword) => ???
      case UserEntity.UserDeletedEvent(id, deletedAt) => ???
      case UserEntity.UserLoginFailureEvent(id, loginAt) => ???
    }
}
