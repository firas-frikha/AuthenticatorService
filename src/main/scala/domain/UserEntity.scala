package domain

import akka.actor.typed.{ActorRef, Behavior}
import akka.persistence.typed.PersistenceId
import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior, ReplyEffect}
import akka.serialization.jackson.CborSerializable

import java.time.{Clock, LocalDateTime, ZoneOffset}

object UserEntity {

  private val MaximumNumberOfLoginAttempts: Int = 3

  sealed trait State extends CborSerializable {
    def id: String

    def applyCommand(command: Command, clock: Clock, tokenGenerator: TokenGenerator): ReplyEffect[Event, State]

    def applyEvent(event: Event): State
  }

  sealed trait Command extends CborSerializable

  case class RegisterUserCommand(firstName: String,
                                 lastName: String,
                                 userId: String,
                                 email: String,
                                 passwordHash: String)
                                (val replyTo: ActorRef[RegisterCommandResult]) extends Command


  case class VerifyUserCommand(verificationToken: String)
                              (val replyTo: ActorRef[VerifyCommandResult]) extends Command

  case class LoginUserCommand(passwordHash: String)
                             (val replyTo: ActorRef[LoginCommandResult]) extends Command

  case class UnlockUserCommand(newPasswordHash: String)
                              (val replyTo: ActorRef[UnlockCommandResult]) extends Command

  case class DeleteUserCommand(replyTo: ActorRef[DeleteCommandResult]) extends Command

  sealed trait Event extends CborSerializable {
    def id: String
  }

  case class RegisteredUserEvent(id: String,
                                 firstName: String,
                                 lastName: String,
                                 userId: String,
                                 email: String,
                                 passwordHash: String,
                                 createdAt: LocalDateTime,
                                 verificationTokenHash: String,
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

  sealed trait VerifyCommandResult extends Result

  case object SuccessfulVerifyCommand extends VerifyCommandResult

  case class UnsupportedVerifyCommand(reason: String) extends VerifyCommandResult

  case object WrongOrExpiredVerificationToken extends VerifyCommandResult

  trait LoginCommandResult extends Result

  case object SuccessfulLoginCommand extends LoginCommandResult

  case class UnsupportedLoginCommand(reason: String) extends LoginCommandResult

  case object FailedLoginCommandResult extends LoginCommandResult

  case object UserLocked extends LoginCommandResult

  trait UnlockCommandResult extends Result

  case object SuccessfulUnlockCommand extends UnlockCommandResult

  case class UnsupportedUnlockCommand(reason: String) extends UnlockCommandResult

  trait DeleteCommandResult extends Result

  case object SuccessfulDeleteCommand extends DeleteCommandResult

  case class UnsupportedDeleteCommand(reason: String) extends DeleteCommandResult

  case class EmptyState(override val id: String) extends State {

    override def applyCommand(command: Command, clock: Clock, tokenGenerator: TokenGenerator): ReplyEffect[Event, State] =
      command match {
        case registerUserCommand: RegisterUserCommand =>
          val timeNow = nowUtc(clock)
          val token = tokenGenerator.generateToken()
          val hashedToken = tokenGenerator.hashToken(token)
          Effect
            .persist(RegisteredUserEvent(
              id = id,
              firstName = registerUserCommand.firstName,
              lastName = registerUserCommand.lastName,
              userId = registerUserCommand.userId,
              passwordHash = registerUserCommand.passwordHash,
              email = registerUserCommand.email,
              createdAt = timeNow,
              tokenExpirationDate = timeNow.plusDays(1),
              verificationTokenHash = hashedToken,
              loginAttempts = 0))
            .thenReply(registerUserCommand.replyTo)(_ => SuccessfulRegisterUserCommand)

        case verifyUserCommand: VerifyUserCommand =>
          Effect
            .reply(verifyUserCommand.replyTo)(UnsupportedVerifyCommand(s"Cannot execute ${classOf[VerifyUserCommand].getSimpleName}, user in ${classOf[EmptyState].getSimpleName} !"))
        case loginUserCommand: LoginUserCommand =>
          Effect
            .reply(loginUserCommand.replyTo)(UnsupportedLoginCommand(s"Cannot execute ${classOf[LoginUserCommand].getSimpleName}, user in ${classOf[EmptyState].getSimpleName} !"))
        case unlockUserCommand: UnlockUserCommand =>
          Effect
            .reply(unlockUserCommand.replyTo)(UnsupportedUnlockCommand(s"Cannot execute ${classOf[UnlockUserCommand].getSimpleName}, user in ${classOf[EmptyState].getSimpleName} !"))
        case deleteUserCommand: DeleteUserCommand =>
          Effect
            .reply(deleteUserCommand.replyTo)(UnsupportedDeleteCommand(s"Cannot execute ${classOf[DeleteUserCommand].getSimpleName}, user in ${classOf[EmptyState].getSimpleName} !"))
      }

    override def applyEvent(event: Event): State =
      event match {
        case registeredUserEvent: RegisteredUserEvent =>
          PendingVerificationState(
            id = registeredUserEvent.id,
            firstName = registeredUserEvent.firstName,
            lastName = registeredUserEvent.lastName,
            userId = registeredUserEvent.userId,
            passwordHash = registeredUserEvent.passwordHash,
            email = registeredUserEvent.email,
            verificationTokenHash = registeredUserEvent.verificationTokenHash,
            tokenExpirationDate = registeredUserEvent.tokenExpirationDate,
            createdAt = registeredUserEvent.createdAt)
        case _: UserVerifiedEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[UserVerifiedEvent].getSimpleName} in ${classOf[EmptyState].getSimpleName} state")
        case _: UserLoggedInEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[UserLoggedInEvent].getSimpleName} in ${classOf[EmptyState].getSimpleName}} state")
        case _: UserLockedEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[UserLockedEvent].getSimpleName} in ${classOf[EmptyState].getSimpleName}} state")
        case _: UserUnlockedEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[UserUnlockedEvent].getSimpleName} in ${classOf[EmptyState].getSimpleName}} state")
        case _: UserLoginFailureEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[UserLoginFailureEvent].getSimpleName} in ${classOf[EmptyState].getSimpleName}} state")
        case _: UserDeletedEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[UserDeletedEvent].getSimpleName} in ${classOf[EmptyState].getSimpleName}} state")
      }
  }

  case class PendingVerificationState(override val id: String,
                                      firstName: String,
                                      lastName: String,
                                      userId: String,
                                      email: String,
                                      createdAt: LocalDateTime,
                                      verificationTokenHash: String,
                                      tokenExpirationDate: LocalDateTime,
                                      passwordHash: String) extends State {

    override def applyCommand(command: Command,
                              clock: Clock,
                              tokenGenerator: TokenGenerator): ReplyEffect[Event, State] =
      command match {
        case registerUserCommand: RegisterUserCommand =>
          Effect.reply(registerUserCommand.replyTo)(UnsupportedRegisterUserCommand(s"Cannot execute ${classOf[RegisterUserCommand].getSimpleName}, user in ${classOf[PendingVerificationState].getSimpleName} !"))
        case verifyUserCommand: VerifyUserCommand =>
          val timeNow = nowUtc(clock)
          val matchResult = tokenGenerator.matches(verifyUserCommand.verificationToken, verificationTokenHash) && timeNow.isBefore(tokenExpirationDate)
          if (matchResult)
            Effect
              .persist(UserVerifiedEvent(id, timeNow))
              .thenReply(verifyUserCommand.replyTo)(_ => SuccessfulVerifyCommand)
          else
            Effect
              .reply(verifyUserCommand.replyTo)(WrongOrExpiredVerificationToken)
        case loginUserCommand: LoginUserCommand =>
          Effect
            .reply(loginUserCommand.replyTo)(UnsupportedLoginCommand(s"Cannot execute ${classOf[LoginUserCommand].getSimpleName}, user in ${classOf[PendingVerificationState].getSimpleName} !"))
        case unlockUserCommand: UnlockUserCommand =>
          Effect
            .reply(unlockUserCommand.replyTo)(UnsupportedUnlockCommand(s"Cannot execute ${classOf[UnlockUserCommand].getSimpleName}, user in ${classOf[PendingVerificationState].getSimpleName} !"))
        case deleteUserCommand: DeleteUserCommand =>
          Effect
            .reply(deleteUserCommand.replyTo)(UnsupportedDeleteCommand(s"Cannot execute ${classOf[DeleteUserCommand].getSimpleName}, user in ${classOf[PendingVerificationState].getSimpleName} !"))
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
        case _: RegisteredUserEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[RegisteredUserEvent].getSimpleName} in ${classOf[PendingVerificationState].getSimpleName} state")
        case _: UserLoggedInEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[UserLoggedInEvent].getSimpleName} in ${classOf[PendingVerificationState].getSimpleName} state")
        case _: UserUnlockedEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[UserUnlockedEvent].getSimpleName} in ${classOf[PendingVerificationState].getSimpleName} state")
        case _: UserLoginFailureEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[UserLoginFailureEvent].getSimpleName} in ${classOf[PendingVerificationState].getSimpleName} state")
        case _: UserLockedEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[UserLockedEvent].getSimpleName} in ${classOf[PendingVerificationState].getSimpleName} state")
        case _: UserDeletedEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[UserDeletedEvent].getSimpleName} in ${classOf[PendingVerificationState].getSimpleName} state")
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

    override def applyCommand(command: Command, clock: Clock, tokenGenerator: TokenGenerator): ReplyEffect[Event, State] =
      command match {
        case registerUserCommand: RegisterUserCommand =>
          Effect
            .reply(registerUserCommand.replyTo)(UnsupportedRegisterUserCommand(s"Cannot execute ${classOf[RegisterUserCommand].getSimpleName}, user in ${classOf[RegisteredUserState].getSimpleName} !"))
        case verifyUserCommand: VerifyUserCommand =>
          Effect
            .reply(verifyUserCommand.replyTo)(UnsupportedVerifyCommand(s"Cannot execute ${classOf[VerifyUserCommand].getSimpleName}, user in ${classOf[RegisteredUserState].getSimpleName} !"))
        case loginUserCommand: LoginUserCommand =>
          val timeNow = nowUtc(clock)
          if (loginUserCommand.passwordHash == passwordHash)
            Effect
              .persist(UserLoggedInEvent(id = id, loggedInAt = timeNow))
              .thenReply(loginUserCommand.replyTo)(_ => SuccessfulLoginCommand)
          else {
            if (loginAttempts + 1 >= MaximumNumberOfLoginAttempts)
              Effect
                .persist(UserLockedEvent(id = id, lockedAt = timeNow))
                .thenReply(loginUserCommand.replyTo)(_ => UserLocked)
            else
              Effect
                .persist(UserLoginFailureEvent(id = id, loginAt = timeNow))
                .thenReply(loginUserCommand.replyTo)(_ => FailedLoginCommandResult)
          }
        case unlockUserCommand: UnlockUserCommand =>
          Effect
            .reply(unlockUserCommand.replyTo)(UnsupportedUnlockCommand(s"Cannot execute ${classOf[UnlockUserCommand].getSimpleName}, user in ${classOf[RegisteredUserState].getSimpleName} !"))
        case deleteUserCommand: DeleteUserCommand =>
          val timeNow = nowUtc(clock)
          Effect
            .persist(UserDeletedEvent(id = id, deletedAt = timeNow))
            .thenReply(deleteUserCommand.replyTo)(_ => SuccessfulDeleteCommand)
      }

    override def applyEvent(event: Event): State =
      event match {
        case _: RegisteredUserEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[RegisteredUserEvent].getSimpleName} in ${classOf[RegisteredUserState].getSimpleName} state")
        case _: UserVerifiedEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[UserVerifiedEvent].getSimpleName} in ${classOf[RegisteredUserState].getSimpleName} state")
        case _: UserLoginFailureEvent =>
          copy(loginAttempts = loginAttempts + 1)
        case _: UserLoggedInEvent =>
          copy(loginAttempts = 0)
        case _: UserUnlockedEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[UserUnlockedEvent].getSimpleName} in ${classOf[RegisteredUserState].getSimpleName} state")

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

    override def applyCommand(command: Command, clock: Clock, tokenGenerator: TokenGenerator): ReplyEffect[Event, State] =
      command match {
        case registerUserCommand: RegisterUserCommand =>
          Effect.reply(registerUserCommand.replyTo)(UnsupportedRegisterUserCommand(s"Cannot execute ${classOf[RegisterUserCommand].getSimpleName}, user in ${classOf[LockedUserState].getSimpleName} !"))
        case verifyUserCommand: VerifyUserCommand =>
          Effect.reply(verifyUserCommand.replyTo)(UnsupportedVerifyCommand(s"Cannot execute ${classOf[VerifyUserCommand].getSimpleName}, user in ${classOf[LockedUserState].getSimpleName} !"))
        case loginUserCommand: LoginUserCommand =>
          Effect.reply(loginUserCommand.replyTo)(UnsupportedLoginCommand(s"Cannot execute ${classOf[LoginUserCommand].getSimpleName}, user in ${classOf[LockedUserState].getSimpleName} !"))
        case unlockUserCommand: UnlockUserCommand =>
          Effect
            .persist(UserUnlockedEvent(id, unlockUserCommand.newPasswordHash))
            .thenReply(unlockUserCommand.replyTo)(_ => SuccessfulUnlockCommand)
        case deleteUserCommand: DeleteUserCommand =>
          Effect
            .reply(deleteUserCommand.replyTo)(UnsupportedDeleteCommand(s"Cannot execute ${classOf[DeleteUserCommand].getSimpleName}, user in ${classOf[LockedUserState].getSimpleName} !"))
      }

    override def applyEvent(event: Event): State =
      event match {
        case _: RegisteredUserEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[RegisteredUserEvent].getSimpleName} in ${classOf[LockedUserState].getSimpleName} state")
        case _: UserVerifiedEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[UserVerifiedEvent].getSimpleName} in ${classOf[LockedUserState].getSimpleName} state")
        case _: UserLoggedInEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[UserLoggedInEvent].getSimpleName} in ${classOf[LockedUserState].getSimpleName} state")
        case _: UserLoginFailureEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[UserLoginFailureEvent].getSimpleName} in ${classOf[LockedUserState].getSimpleName} state")
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
        case _: UserLockedEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[UserLockedEvent].getSimpleName} in ${classOf[LockedUserState].getSimpleName} state")
        case _: UserDeletedEvent =>
          throw new IllegalStateException(s"Unexpected event ${classOf[UserDeletedEvent].getSimpleName} in ${classOf[LockedUserState].getSimpleName} state")
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

    override def applyCommand(command: Command, clock: Clock, tokenGenerator: TokenGenerator): ReplyEffect[Event, State] =
      command match {
        case registerUserCommand: RegisterUserCommand =>
          Effect
            .reply(registerUserCommand.replyTo)(UnsupportedRegisterUserCommand(s"Cannot execute ${classOf[RegisterUserCommand].getSimpleName}, user in ${classOf[DeletedState].getSimpleName} !"))
        case verifyUserCommand: VerifyUserCommand =>
          Effect
            .reply(verifyUserCommand.replyTo)(UnsupportedVerifyCommand(s"Cannot execute ${classOf[VerifyUserCommand].getSimpleName}, user in ${classOf[DeletedState].getSimpleName} !"))
        case loginUserCommand: LoginUserCommand =>
          Effect
            .reply(loginUserCommand.replyTo)(UnsupportedLoginCommand(s"Cannot execute ${classOf[LoginUserCommand].getSimpleName}, user in ${classOf[DeletedState].getSimpleName} !"))
        case unlockUserCommand: UnlockUserCommand =>
          Effect
            .reply(unlockUserCommand.replyTo)(UnsupportedUnlockCommand(s"Cannot execute ${classOf[UnlockUserCommand].getSimpleName}, user in ${classOf[DeletedState].getSimpleName} !"))
        case deleteUserCommand: DeleteUserCommand =>
          Effect
            .reply(deleteUserCommand.replyTo)(UnsupportedDeleteCommand(s"Cannot execute ${classOf[DeleteUserCommand].getSimpleName}, user in ${classOf[DeletedState].getSimpleName} !"))
      }

    override def applyEvent(event: Event): State =
      event match {
        case event => throw new IllegalStateException(s"Unexpected event ${event.getClass.getSimpleName} in ${classOf[DeletedState].getSimpleName} state")
      }
  }

  def apply(persistenceId: PersistenceId,
            entityId: String,
            clock: Clock,
            tokenGenerator: TokenGenerator): Behavior[Command] = EventSourcedBehavior.withEnforcedReplies[Command, Event, State](
    persistenceId = persistenceId,
    emptyState = EmptyState(entityId),
    eventHandler = (state, event) => state.applyEvent(event),
    commandHandler = (state, command) => state.applyCommand(command, clock, tokenGenerator)
  )


  private def nowUtc(clock: Clock): LocalDateTime =
    LocalDateTime.ofInstant(clock.instant(), ZoneOffset.UTC)
}