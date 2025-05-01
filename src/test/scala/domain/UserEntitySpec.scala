package domain

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.persistence.testkit.scaladsl.EventSourcedBehaviorTestKit
import akka.persistence.typed.PersistenceId
import org.scalatest.BeforeAndAfterEach
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID
import scala.util.Random

class UserEntitySpec
  extends ScalaTestWithActorTestKit(EventSourcedBehaviorTestKit.config)
    with AnyWordSpecLike
    with BeforeAndAfterEach {


  val userId: UUID = UUID.randomUUID()
  val eventSourcedTestKit: EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State] = EventSourcedBehaviorTestKit[UserEntity.Command, UserEntity.Event, UserEntity.State](
    system = system,
    behavior = UserEntity(entityId = userId.toString, persistenceId = PersistenceId.ofUniqueId(userId.toString))
  )

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    eventSourcedTestKit.clear()
  }

  "User" must {
    "Receiving registerCommand" must {
      "When user in empty state" in {
        val firstName = Random.alphanumeric.take(12).mkString
        val lastName = Random.alphanumeric.take(12).mkString
        val userId = Random.alphanumeric.take(12).mkString
        val email = Random.alphanumeric.take(12).mkString
        val passwordHash = Random.alphanumeric.take(12).mkString

        val result = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
          firstName = firstName,
          lastName = lastName,
          userId = userId,
          email = email,
          password = passwordHash)
        )

        result.reply shouldBe UserEntity.SuccessfulRegisterUserCommand

        result.eventOfType[UserEntity.RegisteredUserEvent].lastName shouldBe lastName
        result.eventOfType[UserEntity.RegisteredUserEvent].firstName shouldBe firstName
        result.eventOfType[UserEntity.RegisteredUserEvent].userId shouldBe userId
        result.eventOfType[UserEntity.RegisteredUserEvent].email shouldBe email
        result.eventOfType[UserEntity.RegisteredUserEvent].password shouldBe passwordHash

        result.stateOfType[UserEntity.PendingVerificationState].userId shouldBe userId
        result.stateOfType[UserEntity.PendingVerificationState].firstName shouldBe firstName
        result.stateOfType[UserEntity.PendingVerificationState].lastName shouldBe lastName
        result.stateOfType[UserEntity.PendingVerificationState].email shouldBe email
        result.stateOfType[UserEntity.PendingVerificationState].passwordHash shouldBe passwordHash
      }

      "return UnsupportedRegisterCommandUserCommand when user in any other state" in {
        val firstName = Random.alphanumeric.take(12).mkString
        val lastName = Random.alphanumeric.take(12).mkString
        val userId = Random.alphanumeric.take(12).mkString
        val email = Random.alphanumeric.take(12).mkString
        val passwordHash = Random.alphanumeric.take(12).mkString

        val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
          firstName = firstName,
          lastName = lastName,
          userId = userId,
          email = email,
          password = passwordHash)
        )

        result1.reply shouldBe UserEntity.SuccessfulRegisterUserCommand

        result1.eventOfType[UserEntity.RegisteredUserEvent].lastName shouldBe lastName
        result1.eventOfType[UserEntity.RegisteredUserEvent].firstName shouldBe firstName
        result1.eventOfType[UserEntity.RegisteredUserEvent].userId shouldBe userId
        result1.eventOfType[UserEntity.RegisteredUserEvent].email shouldBe email
        result1.eventOfType[UserEntity.RegisteredUserEvent].password shouldBe passwordHash

        result1.stateOfType[UserEntity.PendingVerificationState].userId shouldBe userId
        result1.stateOfType[UserEntity.PendingVerificationState].firstName shouldBe firstName
        result1.stateOfType[UserEntity.PendingVerificationState].lastName shouldBe lastName
        result1.stateOfType[UserEntity.PendingVerificationState].email shouldBe email
        result1.stateOfType[UserEntity.PendingVerificationState].passwordHash shouldBe passwordHash

        val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
          firstName = firstName,
          lastName = lastName,
          userId = userId,
          email = email,
          password = passwordHash)
        )

        result2.reply shouldBe UserEntity.UnsupportedRegisterUserCommand("Unable to register user, user in pending verification state")
      }
    }
    "Receiving VerifyUser command" when {
      "user in empty state" when {
        "Return UnsupportedVerifyUserCommand" in {
          val result = eventSourcedTestKit.runCommand[UserEntity.VerifyUserCommandResult](UserEntity.VerifyUserCommand(verificationToken = Random.alphanumeric.take(12).mkString))

          result.reply shouldBe UserEntity.UnsupportedVerifyUserCommand("Cannot verify User, user is not created yet !")
        }
      }
      "user in PendingVerification state" when {
        "User send correct verification token" when {
          "Return SuccessfulVerifyUserCommand" in {

            val firstName = Random.alphanumeric.take(12).mkString
            val lastName = Random.alphanumeric.take(12).mkString
            val userId = Random.alphanumeric.take(12).mkString
            val email = Random.alphanumeric.take(12).mkString
            val passwordHash = Random.alphanumeric.take(12).mkString

            val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
              firstName = firstName,
              lastName = lastName,
              userId = userId,
              email = email,
              password = passwordHash)
            )

            result1.reply shouldBe UserEntity.SuccessfulRegisterUserCommand

            result1.eventOfType[UserEntity.RegisteredUserEvent].lastName shouldBe lastName
            result1.eventOfType[UserEntity.RegisteredUserEvent].firstName shouldBe firstName
            result1.eventOfType[UserEntity.RegisteredUserEvent].userId shouldBe userId
            result1.eventOfType[UserEntity.RegisteredUserEvent].email shouldBe email
            result1.eventOfType[UserEntity.RegisteredUserEvent].password shouldBe passwordHash

            result1.stateOfType[UserEntity.PendingVerificationState].userId shouldBe userId
            result1.stateOfType[UserEntity.PendingVerificationState].firstName shouldBe firstName
            result1.stateOfType[UserEntity.PendingVerificationState].lastName shouldBe lastName
            result1.stateOfType[UserEntity.PendingVerificationState].email shouldBe email
            result1.stateOfType[UserEntity.PendingVerificationState].passwordHash shouldBe passwordHash

            val pendingVerificationState = result1.stateOfType[UserEntity.PendingVerificationState]

            val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyUserCommandResult](UserEntity.VerifyUserCommand(pendingVerificationState.verificationToken))

            result2.reply shouldBe UserEntity.SuccessfulVerifyUserCommand
            result2.event shouldBe a[UserEntity.UserVerifiedEvent]
          }
        }

        "User send wrong verification token" when {
          "Return WrongOrExpiredVerificationToken" in {

            val firstName = Random.alphanumeric.take(12).mkString
            val lastName = Random.alphanumeric.take(12).mkString
            val userId = Random.alphanumeric.take(12).mkString
            val email = Random.alphanumeric.take(12).mkString
            val passwordHash = Random.alphanumeric.take(12).mkString

            val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
              firstName = firstName,
              lastName = lastName,
              userId = userId,
              email = email,
              password = passwordHash)
            )

            result1.reply shouldBe UserEntity.SuccessfulRegisterUserCommand

            result1.eventOfType[UserEntity.RegisteredUserEvent].lastName shouldBe lastName
            result1.eventOfType[UserEntity.RegisteredUserEvent].firstName shouldBe firstName
            result1.eventOfType[UserEntity.RegisteredUserEvent].userId shouldBe userId
            result1.eventOfType[UserEntity.RegisteredUserEvent].email shouldBe email
            result1.eventOfType[UserEntity.RegisteredUserEvent].password shouldBe passwordHash

            result1.stateOfType[UserEntity.PendingVerificationState].userId shouldBe userId
            result1.stateOfType[UserEntity.PendingVerificationState].firstName shouldBe firstName
            result1.stateOfType[UserEntity.PendingVerificationState].lastName shouldBe lastName
            result1.stateOfType[UserEntity.PendingVerificationState].email shouldBe email
            result1.stateOfType[UserEntity.PendingVerificationState].passwordHash shouldBe passwordHash

            val result2 = eventSourcedTestKit.runCommand[UserEntity.VerifyUserCommandResult](UserEntity.VerifyUserCommand(Random.alphanumeric.take(12).mkString))

            result2.reply shouldBe UserEntity.WrongOrExpiredVerificationToken
          }
        }
      }
    }
  }
}
