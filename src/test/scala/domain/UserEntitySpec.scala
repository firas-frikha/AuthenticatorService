package domain

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.persistence.testkit.scaladsl.EventSourcedBehaviorTestKit
import akka.persistence.typed.PersistenceId
import domain.UserEntity._
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.wordspec.AnyWordSpecLike

import java.time.Clock
import java.util.UUID
import scala.util.Random

class UserEntitySpec
  extends ScalaTestWithActorTestKit(EventSourcedBehaviorTestKit.config)
    with AnyWordSpecLike
    with BeforeAndAfterEach
    with MockFactory {

  "User" must {
    "In empty state" must {
      "receiving registerUserCommand" must {
        "Reply with SuccessfulRegisterUserCommand" in {

          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val firstName = Random.alphanumeric.take(12).mkString
          val lastName = Random.alphanumeric.take(12).mkString
          val userId = Random.alphanumeric.take(12).mkString
          val email = Random.alphanumeric.take(12).mkString
          val passwordHash = Random.alphanumeric.take(12).mkString

          val result = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = firstName,
            lastName = lastName,
            userId = userId,
            email = email,
            passwordHash = passwordHash)
          )

          result.reply shouldBe UserEntity.SuccessfulRegisterUserCommand

          result.event shouldBe a[UserEntity.RegisteredUserEvent]

          result.eventOfType[UserEntity.RegisteredUserEvent].lastName shouldBe lastName
          result.eventOfType[UserEntity.RegisteredUserEvent].firstName shouldBe firstName
          result.eventOfType[UserEntity.RegisteredUserEvent].userId shouldBe userId
          result.eventOfType[UserEntity.RegisteredUserEvent].email shouldBe email
          result.eventOfType[UserEntity.RegisteredUserEvent].passwordHash shouldBe passwordHash

          result.stateOfType[UserEntity.PendingVerificationState].userId shouldBe userId
          result.stateOfType[UserEntity.PendingVerificationState].firstName shouldBe firstName
          result.stateOfType[UserEntity.PendingVerificationState].lastName shouldBe lastName
          result.stateOfType[UserEntity.PendingVerificationState].email shouldBe email
          result.stateOfType[UserEntity.PendingVerificationState].passwordHash shouldBe passwordHash

        }
      }

      "receiving VerifyUserCommand" must {
        "Reply with UnsupportedVerifyUserCommand" in {

          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(verificationToken = Random.alphanumeric.take(12).mkString)
          )
          result.reply shouldBe UserEntity.UnsupportedVerifyCommand(s"Cannot execute ${classOf[VerifyUserCommand].getSimpleName}, user in ${classOf[EmptyState].getSimpleName} !")
          result.hasNoEvents shouldBe true
        }
      }

      "receiving loginUserCommand" must {
        "Reply with UnsupportedLoginCommand" in {
          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32

          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString)
          )
          result.reply shouldBe UserEntity.UnsupportedLoginCommand(s"Cannot execute ${classOf[LoginUserCommand].getSimpleName}, user in ${classOf[EmptyState].getSimpleName} !")
          result.hasNoEvents shouldBe true
        }
      }

      "receiving unlockUserCommand" must {
        "Reply with UnsupportedUnlockCommand" in {
          val entityId: UUID = UUID.randomUUID()

          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result = eventSourcedTestKit.runCommand[UserEntity.UnlockCommandResult](UserEntity.UnlockUserCommand(newPasswordHash = Random.alphanumeric.take(12).mkString)
          )
          result.reply shouldBe UserEntity.UnsupportedUnlockCommand(s"Cannot execute ${classOf[UnlockUserCommand].getSimpleName}, user in ${classOf[EmptyState].getSimpleName} !")
          result.hasNoEvents shouldBe true

        }
      }

      "receiving deleteUserCommand" must {
        "Reply with UnsupportedDeleteCommand" in {
          val entityId: UUID = UUID.randomUUID()

          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]


          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result = eventSourcedTestKit.runCommand[UserEntity.DeleteCommandResult] { replyTo => UserEntity.DeleteUserCommand(replyTo) }
          result.reply shouldBe UserEntity.UnsupportedDeleteCommand(s"Cannot execute ${classOf[DeleteUserCommand].getSimpleName}, user in ${classOf[EmptyState].getSimpleName} !")
          result.hasNoEvents shouldBe true
        }
      }
    }

    "In pending verification state" must {

      "receiving registerUserCommand" must {
        "Reply with UnsupportedRegisterUserCommand" in {

          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val result2 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )
          result2.reply shouldBe UserEntity.UnsupportedRegisterUserCommand(s"Cannot execute ${classOf[RegisterUserCommand].getSimpleName}, user in ${classOf[PendingVerificationState].getSimpleName} !")

          result2.hasNoEvents shouldBe true
        }
      }

      "receiving VerifyUserCommand" must {
        "Reply with UserVerifiedEvent" in {
          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )


          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(generatedToken, verificationTokenHashed)
            .returns(true)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(
            generatedToken))

          result2.reply shouldBe SuccessfulVerifyCommand
          result2.event shouldBe a[UserEntity.UserVerifiedEvent]
        }
        "Reply with WrongOrExpiredVerificationToken" in {
          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash

          val userInputToken = Random.alphanumeric.take(12).mkString

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(userInputToken, verificationTokenHashed)
            .returns(false)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(userInputToken))

          result2.reply shouldBe WrongOrExpiredVerificationToken
          result2.hasNoEvents shouldBe true
        }
      }

      "receiving loginUserCommand" must {
        "Reply with UnsupportedLoginCommand" in {
          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val result2 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString)
          )
          result2.reply shouldBe UserEntity.UnsupportedLoginCommand(s"Cannot execute ${classOf[LoginUserCommand].getSimpleName}, user in ${classOf[PendingVerificationState].getSimpleName} !")
          result2.hasNoEvents shouldBe true
        }
      }

      "receiving unlockUserCommand" must {
        "Reply with UnsupportedUnlockCommand" in {
          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val result2 = eventSourcedTestKit.runCommand[UserEntity.UnlockCommandResult](UserEntity.UnlockUserCommand(newPasswordHash = Random.alphanumeric.take(12).mkString)
          )
          result2.reply shouldBe UserEntity.UnsupportedUnlockCommand(s"Cannot execute ${classOf[UnlockUserCommand].getSimpleName}, user in ${classOf[PendingVerificationState].getSimpleName} !")
          result2.hasNoEvents shouldBe true
        }
      }

      "receiving deleteUserCommand" must {
        "Reply with UnsupportedDeleteCommand" in {
          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val result2 = eventSourcedTestKit.runCommand[UserEntity.DeleteCommandResult] { replyTo => UserEntity.DeleteUserCommand(replyTo) }

          result2.reply shouldBe UserEntity.UnsupportedDeleteCommand(s"Cannot execute ${classOf[DeleteUserCommand].getSimpleName}, user in ${classOf[PendingVerificationState].getSimpleName} !")
          result2.hasNoEvents shouldBe true
        }
      }
    }

    "In registeredUser state" must {

      "receiving registerUserCommand" must {
        "Reply with UnsupportedRegisterUserCommand" in {
          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(generatedToken, verificationTokenHashed)
            .returns(true)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(generatedToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )
          result3.reply shouldBe UserEntity.UnsupportedRegisterUserCommand(s"Cannot execute ${classOf[RegisterUserCommand].getSimpleName}, user in ${classOf[RegisteredUserState].getSimpleName} !")

          result3.hasNoEvents shouldBe true
        }
      }

      "receiving VerifyUserCommand" must {
        "Reply with UnsupportedVerifyCommand" in {

          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(generatedToken, verificationTokenHashed)
            .returns(true)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(
            generatedToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(
            generatedToken))

          result3.reply shouldBe UserEntity.UnsupportedVerifyCommand(s"Cannot execute ${classOf[VerifyUserCommand].getSimpleName}, user in ${classOf[RegisteredUserState].getSimpleName} !")

          result3.hasNoEvents shouldBe true
        }
      }

      "receiving loginUserCommand" must {
        "Reply with SuccessfulLogin" in {
          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash
          val password = result1.eventOfType[UserEntity.RegisteredUserEvent].passwordHash

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(generatedToken, verificationTokenHashed)
            .returns(true)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(generatedToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = password)
          )
          result3.reply shouldBe UserEntity.SuccessfulLoginCommand
          result3.event shouldBe a[UserEntity.UserLoggedInEvent]
        }

        "Reply with FailedLoginResult" in {

          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(generatedToken, verificationTokenHashed)
            .returns(true)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(generatedToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString)
          )
          result3.reply shouldBe UserEntity.FailedLoginCommandResult
          result3.event shouldBe a[UserEntity.UserLoginFailureEvent]
        }

        "Reply with UserLockedEvent" in {

          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(generatedToken, verificationTokenHashed)
            .returns(true)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(generatedToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          result3.reply shouldBe UserEntity.FailedLoginCommandResult
          result3.event shouldBe a[UserEntity.UserLoginFailureEvent]

          val result4 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          result4.reply shouldBe UserEntity.FailedLoginCommandResult
          result4.event shouldBe a[UserEntity.UserLoginFailureEvent]

          val result6 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          result6.reply shouldBe UserEntity.UserLocked
          result6.event shouldBe a[UserEntity.UserLockedEvent]
        }
      }

      "receiving unlockUserCommand" must {
        "Reply with UnsupportedUnlockCommand" in {

          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(generatedToken, verificationTokenHashed)
            .returns(true)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(generatedToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.UnlockCommandResult](UserEntity.UnlockUserCommand(newPasswordHash = Random.alphanumeric.take(12).mkString)
          )
          result3.reply shouldBe UserEntity.UnsupportedUnlockCommand(s"Cannot execute ${classOf[UnlockUserCommand].getSimpleName}, user in ${classOf[RegisteredUserState].getSimpleName} !")
          result3.hasNoEvents shouldBe true
        }
      }

      "receiving deleteUserCommand" must {
        "Reply with UnsupportedDeleteCommand" in {

          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(generatedToken, verificationTokenHashed)
            .returns(true)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(generatedToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.DeleteCommandResult] { replyTo => UserEntity.DeleteUserCommand(replyTo) }

          result3.reply shouldBe UserEntity.SuccessfulDeleteCommand
          result3.event shouldBe a[UserEntity.UserDeletedEvent]
        }
      }
    }

    "In locked state" must {

      "receiving registerUserCommand" must {
        "Reply with UnsupportedRegisterUserCommand" in {

          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(generatedToken, verificationTokenHashed)
            .returns(true)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(generatedToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result4 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result5 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result6 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))

          val result7 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          result7.reply shouldBe UserEntity.UnsupportedRegisterUserCommand(s"Cannot execute ${classOf[RegisterUserCommand].getSimpleName}, user in ${classOf[LockedUserState].getSimpleName} !")

          result7.hasNoEvents shouldBe true
        }
      }

      "receiving VerifyUserCommand" must {
        "Reply with UnsupportedVerifyUserCommand" in {

          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(generatedToken, verificationTokenHashed)
            .returns(true)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(generatedToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result4 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result5 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result6 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))

          val result7 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(generatedToken))

          result7.reply shouldBe UserEntity.UnsupportedVerifyCommand(s"Cannot execute ${classOf[VerifyUserCommand].getSimpleName}, user in ${classOf[LockedUserState].getSimpleName} !")

          result7.hasNoEvents shouldBe true
        }
      }

      "receiving loginUserCommand" must {
        "Reply with UnsupportedLoginCommand" in {

          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(generatedToken, verificationTokenHashed)
            .returns(true)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(generatedToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result4 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result5 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result6 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))

          val result7 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(Random.alphanumeric.take(12).mkString))

          result7.reply shouldBe UserEntity.UnsupportedLoginCommand(s"Cannot execute ${classOf[LoginUserCommand].getSimpleName}, user in ${classOf[LockedUserState].getSimpleName} !")

          result7.hasNoEvents shouldBe true
        }
      }

      "receiving unlockUserCommand" must {
        "Reply with SuccessfulUnlockCommand" in {

          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val userId = Random.alphanumeric.take(12).mkString


          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = userId,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(generatedToken, verificationTokenHashed)
            .returns(true)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(generatedToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result4 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result5 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result6 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))


          val newPassword = Random.alphanumeric.take(12).mkString
          val result7 = eventSourcedTestKit.runCommand[UserEntity.UnlockCommandResult](UserEntity.UnlockUserCommand(newPassword))

          result7.reply shouldBe UserEntity.SuccessfulUnlockCommand

          result7.event shouldBe UserEntity.UserUnlockedEvent(id = entityId.toString, newPassword = newPassword)
        }
      }

      "receiving deleteUserCommand" must {
        "Reply with UnsupportedDeleteCommand" in {

          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )


          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(generatedToken, verificationTokenHashed)
            .returns(true)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(generatedToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result4 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result5 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result6 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))

          val result7 = eventSourcedTestKit.runCommand[UserEntity.DeleteCommandResult] { replyTo => UserEntity.DeleteUserCommand(replyTo) }

          result7.reply shouldBe UserEntity.UnsupportedDeleteCommand(s"Cannot execute ${classOf[DeleteUserCommand].getSimpleName}, user in ${classOf[LockedUserState].getSimpleName} !")

          result7.hasNoEvents shouldBe true
        }
      }
    }

    "In deleted state" must {

      "receiving registerUserCommand" must {
        "Reply with UnsupportedRegisterUserCommand" in {

          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(generatedToken, verificationTokenHashed)
            .returns(true)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(generatedToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.DeleteCommandResult] { replyTo => UserEntity.DeleteUserCommand(replyTo) }

          val result4 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )
          result4.reply shouldBe UserEntity.UnsupportedRegisterUserCommand(s"Cannot execute ${classOf[RegisterUserCommand].getSimpleName}, user in ${classOf[DeletedState].getSimpleName} !")

          result4.hasNoEvents shouldBe true
        }
      }

      "receiving verifyUserCommand" must {
        "Reply with UnsupportedVerifyUserCommand" in {

          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(generatedToken, verificationTokenHashed)
            .returns(true)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(generatedToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.DeleteCommandResult] { replyTo => UserEntity.DeleteUserCommand(replyTo) }

          val result4 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(generatedToken))

          result4.reply shouldBe UserEntity.UnsupportedVerifyCommand(s"Cannot execute ${classOf[VerifyUserCommand].getSimpleName}, user in ${classOf[DeletedState].getSimpleName} !")

          result4.hasNoEvents shouldBe true
        }
      }

      "receiving loginUserCommand" must {
        "Reply with UnsupportedLoginCommand" in {

          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(generatedToken, verificationTokenHashed)
            .returns(true)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(generatedToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.DeleteCommandResult] { replyTo => UserEntity.DeleteUserCommand(replyTo) }

          val result4 = eventSourcedTestKit.runCommand[UserEntity.LoginCommandResult](UserEntity.LoginUserCommand(Random.alphanumeric.take(12).mkString))

          result4.reply shouldBe UserEntity.UnsupportedLoginCommand(s"Cannot execute ${classOf[LoginUserCommand].getSimpleName}, user in ${classOf[DeletedState].getSimpleName} !")

          result4.hasNoEvents shouldBe true
        }
      }

      "receiving unlockUserCommand" must {
        "Reply with UnsupportedUnlockCommand" in {

          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(generatedToken, verificationTokenHashed)
            .returns(true)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(generatedToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.DeleteCommandResult] { replyTo => UserEntity.DeleteUserCommand(replyTo) }

          val result4 = eventSourcedTestKit.runCommand[UserEntity.UnlockCommandResult](UserEntity.UnlockUserCommand(Random.alphanumeric.take(12).mkString))

          result4.reply shouldBe UserEntity.UnsupportedUnlockCommand(s"Cannot execute ${classOf[UnlockUserCommand].getSimpleName}, user in ${classOf[DeletedState].getSimpleName} !")

          result4.hasNoEvents shouldBe true
        }
      }

      "receiving deleteUserCommand" must {
        "Reply with UnsupportedDeleteCommand" in {

          val entityId: UUID = UUID.randomUUID()
          val tokenSize = 32
          val generatedToken = Random.alphanumeric.take(tokenSize).mkString

          val hashedToken = Random.alphanumeric.take(tokenSize).mkString
          val clock: Clock = Clock.systemUTC()
          val tokenGeneratorMock = mock[TokenGenerator]
          (tokenGeneratorMock.generateToken _)
            .expects(32)
            .returns(generatedToken)
          (tokenGeneratorMock.hashToken _)
            .expects(generatedToken)
            .returns(hashedToken)

          val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
            system = system,
            behavior = UserEntity(
              entityId = entityId.toString,
              persistenceId = PersistenceId.ofUniqueId(entityId.toString),
              clock = clock,
              tokenGenerator = tokenGeneratorMock)
          )

          val result1 = eventSourcedTestKit.runCommand[UserEntity.RegisterCommandResult](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationTokenHashed = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationTokenHash

          (tokenGeneratorMock.matches(_: String, _: String))
            .expects(generatedToken, verificationTokenHashed)
            .returns(true)

          val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyCommandResult](UserEntity.VerifyUserCommand(generatedToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.DeleteCommandResult] { replyTo => UserEntity.DeleteUserCommand(replyTo) }

          val result4 = eventSourcedTestKit.runCommand[UserEntity.DeleteCommandResult] { replyTo => UserEntity.DeleteUserCommand(replyTo) }

          result4.reply shouldBe UserEntity.UnsupportedDeleteCommand(s"Cannot execute ${classOf[DeleteUserCommand].getSimpleName}, user in ${classOf[DeletedState].getSimpleName} !")

          result4.hasNoEvents shouldBe true
        }
      }
    }
  }
}