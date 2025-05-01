package domain

import akka.actor.typed.{ActorRef, Behavior}
import akka.persistence.typed.PersistenceId
import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior, ReplyEffect}
import akka.serialization.jackson.CborSerializable

import java.time.LocalDateTime
import java.util.UUID
import scala.util.Random

object UserEntity {

  sealed trait State extends CborSerializable {
    def id: String

    def applyCommand(command: Command): ReplyEffect[Event, State]

    def applyEvent(event: Event): State
  }

  sealed trait Command extends CborSerializable

  case class RegisterUserCommand(firstName: String,
                                 lastName: String,
                                 userId: String,
                                 email: String,
                                 password: String)
                                (val replyTo: ActorRef[RegisterCommandResult]) extends Command


  case class VerifyUserCommand(verificationToken: String)
                              (val replyTo: ActorRef[VerifyUserCommandResult]) extends Command

  sealed trait Event extends CborSerializable {
    def id: String
  }

  case class RegisteredUserEvent(id: String,
                                 firstName: String,
                                 lastName: String,
                                 userId: String,
                                 email: String,
                                 password: String,
                                 createdAt: LocalDateTime,
                                 verificationToken: String,
                                 tokenExpirationDate: LocalDateTime) extends Event

  case class UserVerifiedEvent(id: String,
                               verifiedAt: LocalDateTime) extends Event


  sealed trait Result extends CborSerializable

  sealed trait RegisterCommandResult extends Result

  case object SuccessfulRegisterUserCommand extends RegisterCommandResult

  case class UnsupportedRegisterUserCommand(reason: String) extends RegisterCommandResult

  sealed trait VerifyUserCommandResult extends Result

  case object SuccessfulVerifyUserCommand extends VerifyUserCommandResult

  case class UnsupportedVerifyUserCommand(reason: String) extends VerifyUserCommandResult

  case object WrongOrExpiredVerificationToken extends VerifyUserCommandResult

  case class EmptyState(override val id: String) extends State {

    override def applyCommand(command: Command): ReplyEffect[Event, State] =
      command match {
        case registerUserCommand: RegisterUserCommand =>
          Effect
            .persist(RegisteredUserEvent(
              id = registerUserCommand.userId,
              firstName = registerUserCommand.firstName,
              lastName = registerUserCommand.lastName,
              userId = registerUserCommand.userId,
              password = registerUserCommand.password,
              email = registerUserCommand.email,
              createdAt = LocalDateTime.now(),
              verificationToken = Random.alphanumeric.take(24).mkString,
              tokenExpirationDate = LocalDateTime.now().plusDays(1)))
            .thenReply(registerUserCommand.replyTo)(_ => SuccessfulRegisterUserCommand)

        case verifyUserCommand: VerifyUserCommand =>
          Effect
            .reply(verifyUserCommand.replyTo)(UnsupportedVerifyUserCommand("Cannot verify User, user is not created yet !"))
      }

    override def applyEvent(event: Event): State =
      event match {
        case registeredUserEvent: RegisteredUserEvent =>
          PendingVerificationState(
            id = registeredUserEvent.id,
            firstName = registeredUserEvent.firstName,
            lastName = registeredUserEvent.lastName,
            userId = registeredUserEvent.userId,
            passwordHash = registeredUserEvent.password,
            email = registeredUserEvent.email,
            verificationToken = registeredUserEvent.verificationToken,
            tokenExpirationDate = registeredUserEvent.tokenExpirationDate,
            createdAt = registeredUserEvent.createdAt)
        case userVerifiedEvent: UserVerifiedEvent =>
          throw new IllegalStateException(s"Unexpected event ${userVerifiedEvent.getClass.getName} in ${this.getClass.getName} state")
      }
  }

  case class PendingVerificationState(override val id: String,
                                      firstName: String,
                                      lastName: String,
                                      userId: String,
                                      email: String,
                                      createdAt: LocalDateTime,
                                      verificationToken: String,
                                      tokenExpirationDate: LocalDateTime,
                                      passwordHash: String) extends State {

    override def applyCommand(command: Command): ReplyEffect[Event, State] =
      command match {
        case registerUSerCommand: RegisterUserCommand =>
          Effect.reply(registerUSerCommand.replyTo)(UnsupportedRegisterUserCommand("Unable to register user, user in pending verification state"))
        case verifyUserCommand: VerifyUserCommand =>
          if (verifyUserCommand.verificationToken == verificationToken && LocalDateTime.now().isBefore(tokenExpirationDate))
            Effect
              .persist(UserVerifiedEvent(id, LocalDateTime.now()))
              .thenReply(verifyUserCommand.replyTo)(_ => SuccessfulVerifyUserCommand)
          else
            Effect
              .reply(verifyUserCommand.replyTo)(WrongOrExpiredVerificationToken)
      }

    override def applyEvent(event: Event): State =
      event match {
        case userVerifiedEvent: UserVerifiedEvent =>
          RegisteredUserState(
            id = id,
            firstName = firstName,
            lastName = lastName,
            userId = userId,
            email = email,
            createdAt = createdAt,
            passwordHash = passwordHash,
            verifiedAt = userVerifiedEvent.verifiedAt
          )
        case registeredUserEvent: RegisteredUserEvent =>
          throw new IllegalStateException(s"Unexpected event ${registeredUserEvent.getClass.getName} in ${this.getClass.getName} state")
      }
  }

  case class RegisteredUserState(override val id: String,
                                 firstName: String,
                                 lastName: String,
                                 userId: String,
                                 email: String,
                                 createdAt: LocalDateTime,
                                 passwordHash: String,
                                 verifiedAt: LocalDateTime) extends State {

    override def applyCommand(command: Command): ReplyEffect[Event, State] =
      command match {
        case registerUserCommand: RegisterUserCommand =>
          Effect
            .reply(registerUserCommand.replyTo)(UnsupportedRegisterUserCommand("Unable to register user, user already registered"))
        case verifyUserCommand: VerifyUserCommand =>
          Effect
            .reply(verifyUserCommand.replyTo)(UnsupportedVerifyUserCommand("Unable to register user, user already verified"))
      }

    override def applyEvent(event: Event): State =
      event match {
        case registeredUserEvent: RegisteredUserEvent =>
          throw new IllegalStateException(s"Unexpected event ${registeredUserEvent.getClass.getName} in ${this.getClass.getName} state")
        case userVerifiedEvent: UserVerifiedEvent =>
          throw new IllegalStateException(s"Unexpected event ${userVerifiedEvent.getClass.getName} in ${this.getClass.getName} state")
      }
  }

  case class LockedState(override val id: String) extends State {

    override def applyCommand(command: Command): ReplyEffect[Event, State] = ???

    override def applyEvent(event: Event): State = ???
  }

  case class DeletedState(override val id: String) extends State {

    override def applyCommand(command: Command): ReplyEffect[Event, State] = ???

    override def applyEvent(event: Event): State = ???
  }

  def apply(persistenceId: PersistenceId,
            entityId: String): Behavior[Command] = EventSourcedBehavior[Command, Event, State](
    persistenceId = persistenceId,
    emptyState = EmptyState(entityId),
    eventHandler = (state, event) => state.applyEvent(event),
    commandHandler = (state, command) => state.applyCommand(command)
  )
}