package domain

import akka.actor.typed.{ActorRef, Behavior}
import akka.persistence.typed.PersistenceId
import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior, ReplyEffect}
import akka.serialization.jackson.CborSerializable

import java.time.LocalDateTime
import scala.util.Random

object UserEntity {

  private val MaximumNumberOfLoginAttempts: Int = 3

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

  case class LoginUserCommand(passwordHash: String)
                             (val replyTo: ActorRef[LoginResult]) extends Command

  case class UnlockUserCommand(newPasswordHash: String)
                              (val replyTo: ActorRef[UnlockResult]) extends Command

  case class DeleteUserCommand()
                              (val replyTo: ActorRef[DeleteUserResult]) extends Command

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
                                 tokenExpirationDate: LocalDateTime,
                                 loginAttempts: Int) extends Event

  case class UserVerifiedEvent(id: String,
                               verifiedAt: LocalDateTime) extends Event

  case class UserLockedEvent(id: String, lockedAt: LocalDateTime) extends Event

  case class UserLoggedInEvent(id: String,
                               loggedInAt: LocalDateTime) extends Event

  case class UserUnlockedEvent(id: String,
                               newPassword: String) extends Event


  case class UserDeletedEvent(id: String,
                              deletedAt: LocalDateTime) extends Event


  case class UserLoginFailureEvent(id: String, loginAt: LocalDateTime) extends Event

  sealed trait Result extends CborSerializable

  sealed trait RegisterCommandResult extends Result

  case object SuccessfulRegisterUserCommand extends RegisterCommandResult

  case class UnsupportedRegisterUserCommand(reason: String) extends RegisterCommandResult

  sealed trait VerifyUserCommandResult extends Result

  case object SuccessfulVerifyUserCommand extends VerifyUserCommandResult

  case class UnsupportedVerifyUserCommand(reason: String) extends VerifyUserCommandResult

  case object WrongOrExpiredVerificationToken extends VerifyUserCommandResult

  trait LoginResult extends Result

  case object SuccessfulLogin extends LoginResult

  case class UnsupportedLoginCommand(reason: String) extends LoginResult

  case object FailedLoginResult extends LoginResult

  case object UserLocked extends LoginResult

  trait UnlockResult extends Result

  case object SuccessfulUnlockCommand extends UnlockResult

  case class UnsupportedUnlockCommand(reason: String) extends UnlockResult

  trait DeleteUserResult extends Result

  case object SuccessfulDeleteCommand extends DeleteUserResult

  case class UnsupportedDeleteCommand(reason: String) extends DeleteUserResult

  case class EmptyState(override val id: String) extends State {

    override def applyCommand(command: Command): ReplyEffect[Event, State] =
      command match {
        case registerUserCommand: RegisterUserCommand =>
          Effect
            .persist(RegisteredUserEvent(
              id = id,
              firstName = registerUserCommand.firstName,
              lastName = registerUserCommand.lastName,
              userId = registerUserCommand.userId,
              password = registerUserCommand.password,
              email = registerUserCommand.email,
              createdAt = LocalDateTime.now(),
              verificationToken = Random.alphanumeric.take(24).mkString,
              tokenExpirationDate = LocalDateTime.now().plusDays(1),
              loginAttempts = 0))
            .thenReply(registerUserCommand.replyTo)(_ => SuccessfulRegisterUserCommand)

        case verifyUserCommand: VerifyUserCommand =>
          Effect
            .reply(verifyUserCommand.replyTo)(UnsupportedVerifyUserCommand(s"Cannot execute ${VerifyUserCommand.getClass.getSimpleName}, user in ${EmptyState.getClass.getSimpleName} !"))
        case loginUserCommand: LoginUserCommand =>
          Effect
            .reply(loginUserCommand.replyTo)(UnsupportedLoginCommand(s"Cannot execute ${LoginUserCommand.getClass.getSimpleName}, user in ${EmptyState.getClass.getSimpleName} !"))
        case unlockUserCommand: UnlockUserCommand =>
          Effect
            .reply(unlockUserCommand.replyTo)(UnsupportedUnlockCommand(s"Cannot execute ${UnlockUserCommand.getClass.getSimpleName}, user in ${EmptyState.getClass.getSimpleName} !"))
        case deleteUserCommand: DeleteUserCommand =>
          Effect
            .reply(deleteUserCommand.replyTo)(UnsupportedDeleteCommand(s"Cannot execute ${DeleteUserCommand.getClass.getSimpleName}, user in ${EmptyState.getClass.getSimpleName} !"))
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
        case userLoggedInEvent: UserLoggedInEvent =>
          throw new IllegalStateException(s"Unexpected event ${userLoggedInEvent.getClass.getName} in ${this.getClass.getName} state")
        case userLockedEvent: UserLockedEvent =>
          throw new IllegalStateException(s"Unexpected event ${userLockedEvent.getClass.getName} in ${this.getClass.getName} state")
        case userUnlockedEvent: UserUnlockedEvent =>
          throw new IllegalStateException(s"Unexpected event ${userUnlockedEvent.getClass.getName} in ${this.getClass.getName} state")
        case userLoginFailureEvent: UserLoginFailureEvent =>
          throw new IllegalStateException(s"Unexpected event ${userLoginFailureEvent.getClass.getName} in ${this.getClass.getName} state")
        case userDeletedEvent: UserDeletedEvent =>
          throw new IllegalStateException(s"Unexpected event ${userDeletedEvent.getClass.getName} in ${this.getClass.getName} state")
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
        case registerUserCommand: RegisterUserCommand =>
          Effect.reply(registerUserCommand.replyTo)(UnsupportedRegisterUserCommand(s"Cannot execute ${RegisterUserCommand.getClass.getSimpleName}, user in ${PendingVerificationState.getClass.getSimpleName} !"))
        case verifyUserCommand: VerifyUserCommand =>
          if (verifyUserCommand.verificationToken == verificationToken && LocalDateTime.now().isBefore(tokenExpirationDate))
            Effect
              .persist(UserVerifiedEvent(id, LocalDateTime.now()))
              .thenReply(verifyUserCommand.replyTo)(_ => SuccessfulVerifyUserCommand)
          else
            Effect
              .reply(verifyUserCommand.replyTo)(WrongOrExpiredVerificationToken)
        case loginUserCommand: LoginUserCommand =>
          Effect
            .reply(loginUserCommand.replyTo)(UnsupportedLoginCommand(s"Cannot execute ${LoginUserCommand.getClass.getSimpleName}, user in ${PendingVerificationState.getClass.getSimpleName} !"))
        case unlockUserCommand: UnlockUserCommand =>
          Effect
            .reply(unlockUserCommand.replyTo)(UnsupportedUnlockCommand(s"Cannot execute ${UnlockUserCommand.getClass.getSimpleName}, user in ${PendingVerificationState.getClass.getSimpleName} !"))
        case deleteUserCommand: DeleteUserCommand =>
          Effect
            .reply(deleteUserCommand.replyTo)(UnsupportedDeleteCommand(s"Cannot execute ${DeleteUserCommand.getClass.getSimpleName}, user in ${PendingVerificationState.getClass.getSimpleName} !"))
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
            verifiedAt = userVerifiedEvent.verifiedAt,
            loginAttempts = 0)
        case registeredUserEvent: RegisteredUserEvent =>
          throw new IllegalStateException(s"Unexpected event ${registeredUserEvent.getClass.getName} in ${this.getClass.getName} state")
        case userLoggedInEvent: UserLoggedInEvent =>
          throw new IllegalStateException(s"Unexpected event ${userLoggedInEvent.getClass.getName} in ${this.getClass.getName} state")
        case userUnlockedEvent: UserUnlockedEvent =>
          throw new IllegalStateException(s"Unexpected event ${userUnlockedEvent.getClass.getName} in ${this.getClass.getName} state")
        case userLoginFailureEvent: UserLoginFailureEvent =>
          throw new IllegalStateException(s"Unexpected event ${userLoginFailureEvent.getClass.getName} in ${this.getClass.getName} state")
        case userLockedEvent: UserLockedEvent =>
          throw new IllegalStateException(s"Unexpected event ${userLockedEvent.getClass.getName} in ${this.getClass.getName} state")
        case userDeletedEvent: UserDeletedEvent =>
          throw new IllegalStateException(s"Unexpected event ${userDeletedEvent.getClass.getName} in ${this.getClass.getName} state")
      }
  }

  case class RegisteredUserState(override val id: String,
                                 firstName: String,
                                 lastName: String,
                                 userId: String,
                                 email: String,
                                 createdAt: LocalDateTime,
                                 passwordHash: String,
                                 verifiedAt: LocalDateTime,
                                 loginAttempts: Int) extends State {

    override def applyCommand(command: Command): ReplyEffect[Event, State] =
      command match {
        case registerUserCommand: RegisterUserCommand =>
          Effect
            .reply(registerUserCommand.replyTo)(UnsupportedRegisterUserCommand(s"Cannot execute ${RegisterUserCommand.getClass.getSimpleName}, user in ${RegisteredUserState.getClass.getSimpleName} !"))
        case verifyUserCommand: VerifyUserCommand =>
          Effect
            .reply(verifyUserCommand.replyTo)(UnsupportedVerifyUserCommand(s"Cannot execute ${RegisterUserCommand.getClass.getSimpleName}, user in ${RegisteredUserState.getClass.getSimpleName} !"))
        case loginUserCommand: LoginUserCommand =>
          if (loginUserCommand.passwordHash == passwordHash)
            Effect
              .persist(UserLoggedInEvent(id = id, loggedInAt = LocalDateTime.now()))
              .thenReply(loginUserCommand.replyTo)(_ => SuccessfulLogin)
          else {
            if (loginAttempts + 1 <= MaximumNumberOfLoginAttempts)
              Effect
                .persist(UserLoginFailureEvent(id = id, loginAt = LocalDateTime.now()))
                .thenReply(loginUserCommand.replyTo)(_ => FailedLoginResult)
            else
              Effect
                .persist(UserLockedEvent(id = id, lockedAt = LocalDateTime.now()))
                .thenReply(loginUserCommand.replyTo)(_ => UserLocked)
          }
        case unlockUserCommand: UnlockUserCommand =>
          Effect
            .reply(unlockUserCommand.replyTo)(UnsupportedUnlockCommand(s"Cannot execute ${UnlockUserCommand.getClass.getSimpleName}, user in ${RegisteredUserState.getClass.getSimpleName} !"))
        case deleteUserCommand: DeleteUserCommand =>
          Effect
            .persist(UserDeletedEvent(id = id, deletedAt = LocalDateTime.now()))
            .thenReply(deleteUserCommand.replyTo)(_ => SuccessfulDeleteCommand)
      }

    override def applyEvent(event: Event): State =
      event match {
        case registeredUserEvent: RegisteredUserEvent =>
          throw new IllegalStateException(s"Unexpected event ${registeredUserEvent.getClass.getName} in ${this.getClass.getName} state")
        case userVerifiedEvent: UserVerifiedEvent =>
          throw new IllegalStateException(s"Unexpected event ${userVerifiedEvent.getClass.getName} in ${this.getClass.getName} state")
        case _: UserLoginFailureEvent =>
          copy(loginAttempts = loginAttempts + 1)
        case _: UserLoggedInEvent =>
          copy(loginAttempts = 0)
        case userUnlockedEvent: UserUnlockedEvent =>
          throw new IllegalStateException(s"Unexpected event ${userUnlockedEvent.getClass.getName} in ${this.getClass.getName} state")

        case userLockedEvent: UserLockedEvent =>
          LockedUserState(id = id,
            firstName = firstName,
            lastName = lastName,
            userId = userId,
            email = email,
            createdAt = createdAt,
            passwordHash = passwordHash,
            verifiedAt = verifiedAt,
            lockedAt = userLockedEvent.lockedAt)

        case userDeletedEvent: UserDeletedEvent =>
          DeletedState(
            id = id,
            firstName = firstName,
            lastName = lastName,
            userId = userId,
            email = email,
            createdAt = createdAt,
            passwordHash = passwordHash,
            verifiedAt = verifiedAt,
            deletedAt = userDeletedEvent.deletedAt
          )
      }
  }

  case class LockedUserState(override val id: String,
                             firstName: String,
                             lastName: String,
                             userId: String,
                             email: String,
                             createdAt: LocalDateTime,
                             passwordHash: String,
                             verifiedAt: LocalDateTime,
                             lockedAt: LocalDateTime) extends State {

    override def applyCommand(command: Command): ReplyEffect[Event, State] =
      command match {
        case registerUserCommand: RegisterUserCommand =>
          Effect.reply(registerUserCommand.replyTo)(UnsupportedRegisterUserCommand(s"Cannot execute ${RegisterUserCommand.getClass.getSimpleName}, user in ${LockedUserState.getClass.getSimpleName} !"))
        case verifyUserCommand: VerifyUserCommand =>
          Effect.reply(verifyUserCommand.replyTo)(UnsupportedVerifyUserCommand(s"Cannot execute ${VerifyUserCommand.getClass.getSimpleName}, user in ${LockedUserState.getClass.getSimpleName} !"))
        case loginUserCommand: LoginUserCommand =>
          Effect.reply(loginUserCommand.replyTo)(UnsupportedLoginCommand(s"Cannot execute ${LoginUserCommand.getClass.getSimpleName}, user in ${LockedUserState.getClass.getSimpleName} !"))
        case unlockUserCommand: UnlockUserCommand =>
          Effect
            .persist(UserUnlockedEvent(id, unlockUserCommand.newPasswordHash))
            .thenReply(unlockUserCommand.replyTo)(_ => SuccessfulUnlockCommand)
        case deleteUserCommand: DeleteUserCommand =>
          Effect
            .reply(deleteUserCommand.replyTo)(UnsupportedDeleteCommand(s"Cannot execute ${DeleteUserCommand.getClass.getSimpleName}, user in ${LockedUserState.getClass.getSimpleName} !"))
      }

    override def applyEvent(event: Event): State =
      event match {
        case registeredUserEvent: RegisteredUserEvent =>
          throw new IllegalStateException(s"Unexpected event ${registeredUserEvent.getClass.getName} in ${this.getClass.getName} state")
        case userVerifiedEvent: UserVerifiedEvent =>
          throw new IllegalStateException(s"Unexpected event ${userVerifiedEvent.getClass.getName} in ${this.getClass.getName} state")
        case userLoggedInEvent: UserLoggedInEvent =>
          throw new IllegalStateException(s"Unexpected event ${userLoggedInEvent.getClass.getName} in ${this.getClass.getName} state")
        case userLoginFailureEvent: UserLoginFailureEvent =>
          throw new IllegalStateException(s"Unexpected event ${userLoginFailureEvent.getClass.getName} in ${this.getClass.getName} state")
        case userUnlockedEvent: UserUnlockedEvent =>
          RegisteredUserState(
            id = id,
            firstName = firstName,
            lastName = lastName,
            userId = userId,
            email = email,
            createdAt = createdAt,
            passwordHash = userUnlockedEvent.newPassword,
            verifiedAt = verifiedAt,
            loginAttempts = 0
          )
        case userLockedEvent: UserLockedEvent =>
          throw new IllegalStateException(s"Unexpected event ${userLockedEvent.getClass.getName} in ${this.getClass.getName} state")
        case userDeletedEvent: UserDeletedEvent =>
          throw new IllegalStateException(s"Unexpected event ${userDeletedEvent.getClass.getName} in ${this.getClass.getName} state")
      }
  }

  case class DeletedState(override val id: String,
                          firstName: String,
                          lastName: String,
                          userId: String,
                          email: String,
                          createdAt: LocalDateTime,
                          passwordHash: String,
                          verifiedAt: LocalDateTime,
                          deletedAt: LocalDateTime) extends State {

    override def applyCommand(command: Command): ReplyEffect[Event, State] =
      command match {
        case registerUserCommand: RegisterUserCommand =>
          Effect
            .reply(registerUserCommand.replyTo)(UnsupportedRegisterUserCommand(s"Cannot execute ${UserEntity.RegisterUserCommand.getClass.getSimpleName}, user in ${UserEntity.DeletedState.getClass.getSimpleName} !"))
        case verifyUserCommand: VerifyUserCommand =>
          Effect
            .reply(verifyUserCommand.replyTo)(UnsupportedVerifyUserCommand(s"Cannot execute ${UserEntity.VerifyUserCommand.getClass.getSimpleName}, user in ${UserEntity.DeletedState.getClass.getSimpleName} !"))
        case loginUserCommand: LoginUserCommand =>
          Effect
            .reply(loginUserCommand.replyTo)(UnsupportedLoginCommand(s"Cannot execute ${UserEntity.LoginUserCommand.getClass.getSimpleName}, user in ${UserEntity.DeletedState.getClass.getSimpleName} !"))
        case unlockUserCommand: UnlockUserCommand =>
          Effect
            .reply(unlockUserCommand.replyTo)(UnsupportedUnlockCommand(s"Cannot execute ${UserEntity.UnlockUserCommand.getClass.getSimpleName}, user in ${UserEntity.DeletedState.getClass.getSimpleName} !"))
        case deleteUserCommand: DeleteUserCommand =>
          Effect
            .reply(deleteUserCommand.replyTo)(UnsupportedDeleteCommand(s"Cannot execute ${UserEntity.DeleteUserCommand.getClass.getSimpleName}, user in ${UserEntity.DeletedState.getClass.getSimpleName} !"))
      }

    override def applyEvent(event: Event): State =
      event match {
        case event => throw new IllegalStateException(s"Unexpected event ${event.getClass.getName} in ${this.getClass.getName} state")
      }
  }

  def apply(persistenceId: PersistenceId,
            entityId: String): Behavior[Command] = EventSourcedBehavior[Command, Event, State](
    persistenceId = persistenceId,
    emptyState = EmptyState(entityId),
    eventHandler = (state, event) => state.applyEvent(event),
    commandHandler = (state, command) => state.applyCommand(command)
  )
}