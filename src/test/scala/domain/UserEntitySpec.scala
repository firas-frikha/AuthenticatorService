package domain

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.persistence.testkit.scaladsl.EventSourcedBehaviorTestKit
import akka.persistence.typed.PersistenceId
import domain.UserEntity.{SuccessfulVerifyUserCommand, WrongOrExpiredVerificationToken}
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
    "In empty state" must {
      "receiving registerUserCommand" must {
        "Reply with SuccessfulRegisterUserCommand" in {
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
          val result = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken = Random.alphanumeric.take(12).mkString)
          )
          result.reply shouldBe UserEntity.UnsupportedVerifyUserCommand(s"Cannot execute ${UserEntity.VerifyUserCommand.getClass.getSimpleName}, user in ${UserEntity.EmptyState.getClass.getSimpleName} !")
          result.hasNoEvents shouldBe true
        }
      }

      "receiving loginUserCommand" must {
        "Reply with UnsupportedVerifyUserCommand" in {
          val result = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString)
          )
          result.reply shouldBe UserEntity.UnsupportedLoginCommand(s"Cannot execute ${UserEntity.LoginUserCommand.getClass.getSimpleName}, user in ${UserEntity.EmptyState.getClass.getSimpleName} !")
          result.hasNoEvents shouldBe true
        }
      }

      "receiving unlockUserCommand" must {
        "Reply with UnsupportedUnlockCommand" in {
          val result = eventSourcedTestKit.runCommand[UserEntity.UnlockResult](UserEntity.UnlockUserCommand(newPasswordHash = Random.alphanumeric.take(12).mkString)
          )
          result.reply shouldBe UserEntity.UnsupportedUnlockCommand(s"Cannot execute ${UserEntity.UnlockUserCommand.getClass.getSimpleName}, user in ${UserEntity.EmptyState.getClass.getSimpleName} !")
          result.hasNoEvents shouldBe true

        }
      }

      "receiving deleteUserCommand" must {
        "Reply with UnsupportedUnlockCommand" in {
          val result = eventSourcedTestKit.runCommand[UserEntity.DeleteUserResult](UserEntity.DeleteUserCommand()
          )
          result.reply shouldBe UserEntity.UnsupportedDeleteCommand(s"Cannot execute ${UserEntity.DeleteUserCommand.getClass.getSimpleName}, user in ${UserEntity.EmptyState.getClass.getSimpleName} !")
          result.hasNoEvents shouldBe true
        }
      }
    }

    "In pending verification state" must {

      "receiving registerUserCommand" must {
        "Reply with UnsupportedRegisterUserCommand" in {

          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )
          result2.reply shouldBe UserEntity.UnsupportedRegisterUserCommand(s"Cannot execute ${UserEntity.RegisterUserCommand.getClass.getSimpleName}, user in ${UserEntity.PendingVerificationState.getClass.getSimpleName} !")

          result2.hasNoEvents shouldBe true
        }
      }

      "receiving VerifyUserCommand" must {
        "Reply with UserVerifiedEvent" in {
          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationToken = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationToken

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(
            verificationToken))

          result2.reply shouldBe SuccessfulVerifyUserCommand
          result2.event shouldBe a[UserEntity.UserVerifiedEvent]
        }
        "Reply with WrongOrExpiredVerificationToken" in {
          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )


          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(Random.alphanumeric.take(12).mkString))

          result2.reply shouldBe WrongOrExpiredVerificationToken
          result2.hasNoEvents shouldBe true
        }
      }

      "receiving loginUserCommand" must {
        "Reply with UnsupportedVerifyUserCommand" in {
          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val result2 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString)
          )
          result2.reply shouldBe UserEntity.UnsupportedLoginCommand(s"Cannot execute ${UserEntity.LoginUserCommand.getClass.getSimpleName}, user in ${UserEntity.PendingVerificationState.getClass.getSimpleName} !")
          result2.hasNoEvents shouldBe true
        }
      }

      "receiving unlockUserCommand" must {
        "Reply with UnsupportedUnlockCommand" in {
          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.UnlockUserCommand(newPasswordHash = Random.alphanumeric.take(12).mkString)
          )
          result2.reply shouldBe UserEntity.UnsupportedUnlockCommand(s"Cannot execute ${UserEntity.UnlockUserCommand.getClass.getSimpleName}, user in ${UserEntity.PendingVerificationState.getClass.getSimpleName} !")
          result2.hasNoEvents shouldBe true
        }
      }

      "receiving deleteUserCommand" must {
        "Reply with UnsupportedDeleteCommand" in {
          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.DeleteUserCommand())

          result2.reply shouldBe UserEntity.UnsupportedDeleteCommand(s"Cannot execute ${UserEntity.DeleteUserCommand.getClass.getSimpleName}, user in ${UserEntity.PendingVerificationState.getClass.getSimpleName} !")
          result2.hasNoEvents shouldBe true
        }
      }
    }

    "In registeredUser state" must {

      "receiving registerUserCommand" must {
        "Reply with UnsupportedRegisterUserCommand" in {

          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationToken = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationToken

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )
          result3.reply shouldBe UserEntity.UnsupportedRegisterUserCommand(s"Cannot execute ${UserEntity.RegisterUserCommand.getClass.getSimpleName}, user in ${UserEntity.RegisteredUserState.getClass.getSimpleName} !")

          result3.hasNoEvents shouldBe true
        }
      }

      "receiving VerifyUserCommand" must {
        "Reply with UserVerifiedEvent" in {
          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationToken = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationToken

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(
            verificationToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(
            verificationToken))

          result3.reply shouldBe UserEntity.UnsupportedVerifyUserCommand(s"Cannot execute ${UserEntity.RegisterUserCommand.getClass.getSimpleName}, user in ${UserEntity.RegisteredUserState.getClass.getSimpleName} !")

          result3.hasNoEvents shouldBe true
        }
      }

      "receiving loginUserCommand" must {
        "Reply with SuccessfulLogin" in {

          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationToken = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationToken
          val password = result1.eventOfType[UserEntity.RegisteredUserEvent].passwordHash

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = password)
          )
          result3.reply shouldBe UserEntity.SuccessfulLogin
          result3.event shouldBe a[UserEntity.UserLoggedInEvent]
        }

        "Reply with FailedLoginResult" in {

          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationToken = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationToken

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString)
          )
          result3.reply shouldBe UserEntity.FailedLoginResult
          result3.event shouldBe a[UserEntity.UserLoginFailureEvent]
        }

        "Reply with UserLockedEvent" in {

          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationToken = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationToken

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          result3.reply shouldBe UserEntity.FailedLoginResult
          result3.event shouldBe a[UserEntity.UserLoginFailureEvent]

          val result4 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          result4.reply shouldBe UserEntity.FailedLoginResult
          result4.event shouldBe a[UserEntity.UserLoginFailureEvent]


          val result5 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          result5.reply shouldBe UserEntity.FailedLoginResult
          result5.event shouldBe a[UserEntity.UserLoginFailureEvent]


          val result6 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          result6.reply shouldBe UserEntity.UserLocked
          result6.event shouldBe a[UserEntity.UserLockedEvent]
        }
      }

      "receiving unlockUserCommand" must {
        "Reply with UnsupportedUnlockCommand" in {
          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationToken = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationToken

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken))


          val result3 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.UnlockUserCommand(newPasswordHash = Random.alphanumeric.take(12).mkString)
          )
          result3.reply shouldBe UserEntity.UnsupportedUnlockCommand(s"Cannot execute ${UserEntity.UnlockUserCommand.getClass.getSimpleName}, user in ${UserEntity.RegisteredUserState.getClass.getSimpleName} !")
          result3.hasNoEvents shouldBe true
        }
      }

      "receiving deleteUserCommand" must {
        "Reply with UnsupportedDeleteCommand" in {
          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )
          val verificationToken = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationToken

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.DeleteUserCommand())

          result3.reply shouldBe UserEntity.SuccessfulDeleteCommand
          result3.event shouldBe a[UserEntity.UserDeletedEvent]
        }
      }
    }

    "In locked state" must {

      "receiving registerUserCommand" must {
        "Reply with UnsupportedRegisterUserCommand" in {

          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationToken = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationToken

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result4 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result5 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result6 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))

          val result7 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          result7.reply shouldBe UserEntity.UnsupportedRegisterUserCommand(s"Cannot execute ${UserEntity.RegisterUserCommand.getClass.getSimpleName}, user in ${UserEntity.LockedUserState.getClass.getSimpleName} !")

          result7.hasNoEvents shouldBe true
        }
      }

      "receiving VerifyUserCommand" must {
        "Reply with UnsupportedVerifyUserCommand" in {
          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationToken = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationToken

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result4 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result5 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result6 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))

          val result7 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken))

          result7.reply shouldBe UserEntity.UnsupportedVerifyUserCommand(s"Cannot execute ${UserEntity.VerifyUserCommand.getClass.getSimpleName}, user in ${UserEntity.LockedUserState.getClass.getSimpleName} !")

          result7.hasNoEvents shouldBe true
        }
      }

      "receiving loginUserCommand" must {
        "Reply with UnsupportedLoginUserCommand" in {
          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationToken = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationToken

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result4 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result5 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result6 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))

          val result7 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.LoginUserCommand(Random.alphanumeric.take(12).mkString))

          result7.reply shouldBe UserEntity.UnsupportedLoginCommand(s"Cannot execute ${UserEntity.LoginUserCommand.getClass.getSimpleName}, user in ${UserEntity.LockedUserState.getClass.getSimpleName} !")

          result7.hasNoEvents shouldBe true
        }
      }

      "receiving unlockUserCommand" must {
        "Reply with SuccessfulUnlockCommand" in {
          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationToken = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationToken

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result4 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result5 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result6 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))


          val newPassword = Random.alphanumeric.take(12).mkString
          val result7 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.UnlockUserCommand(newPassword))

          result7.reply shouldBe UserEntity.SuccessfulUnlockCommand

          result7.event shouldBe UserEntity.UserUnlockedEvent(id = userId.toString, newPassword = newPassword)
        }
      }

      "receiving deleteUserCommand" must {
        "Reply with UnsupportedDeletedCommand" in {
          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationToken = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationToken

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result4 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result5 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))
          val result6 = eventSourcedTestKit.runCommand[UserEntity.LoginResult](UserEntity.LoginUserCommand(passwordHash = Random.alphanumeric.take(12).mkString))

          val result7 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.DeleteUserCommand())

          result7.reply shouldBe UserEntity.UnsupportedDeleteCommand(s"Cannot execute ${UserEntity.DeleteUserCommand.getClass.getSimpleName}, user in ${UserEntity.LockedUserState.getClass.getSimpleName} !")

          result7.hasNoEvents shouldBe true
        }
      }
    }

    "In deleted state" must {

      "receiving registerUserCommand" must {
        "Reply with UnsupportedRegisterUserCommand" in {

          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationToken = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationToken

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.DeleteUserCommand())

          val result4 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )
          result4.reply shouldBe UserEntity.UnsupportedRegisterUserCommand(s"Cannot execute ${UserEntity.RegisterUserCommand.getClass.getSimpleName}, user in ${UserEntity.DeletedState.getClass.getSimpleName} !")

          result4.hasNoEvents shouldBe true
        }
      }

      "receiving verifyUserCommand" must {
        "Reply with UnsupportedVerifyUserCommand" in {

          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationToken = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationToken

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.DeleteUserCommand())

          val result4 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken))

          result4.reply shouldBe UserEntity.UnsupportedVerifyUserCommand(s"Cannot execute ${UserEntity.VerifyUserCommand.getClass.getSimpleName}, user in ${UserEntity.DeletedState.getClass.getSimpleName} !")

          result4.hasNoEvents shouldBe true
        }
      }

      "receiving loginUserCommand" must {
        "Reply with UnsupportedLoginCommand" in {

          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationToken = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationToken

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.DeleteUserCommand())

          val result4 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.LoginUserCommand(Random.alphanumeric.take(12).mkString))

          result4.reply shouldBe UserEntity.UnsupportedLoginCommand(s"Cannot execute ${UserEntity.LoginUserCommand.getClass.getSimpleName}, user in ${UserEntity.DeletedState.getClass.getSimpleName} !")

          result4.hasNoEvents shouldBe true
        }
      }

      "receiving unlockUserCommand" must {
        "Reply with UnsupportedUnlockCommand" in {

          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationToken = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationToken

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.DeleteUserCommand())

          val result4 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.UnlockUserCommand(Random.alphanumeric.take(12).mkString))

          result4.reply shouldBe UserEntity.UnsupportedUnlockCommand(s"Cannot execute ${UserEntity.UnlockUserCommand.getClass.getSimpleName}, user in ${UserEntity.DeletedState.getClass.getSimpleName} !")

          result4.hasNoEvents shouldBe true
        }
      }

      "receiving deleteUserCommand" must {
        "Reply with UnsupportedDeleteCommand" in {

          val result1 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.RegisterUserCommand(
            firstName = Random.alphanumeric.take(12).mkString,
            lastName = Random.alphanumeric.take(12).mkString,
            userId = Random.alphanumeric.take(12).mkString,
            email = Random.alphanumeric.take(12).mkString,
            passwordHash = Random.alphanumeric.take(12).mkString)
          )

          val verificationToken = result1.eventOfType[UserEntity.RegisteredUserEvent].verificationToken

          val result2 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.VerifyUserCommand(verificationToken))

          val result3 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.DeleteUserCommand())

          val result4 = eventSourcedTestKit.runCommand[UserEntity.Result](UserEntity.DeleteUserCommand())

          result4.reply shouldBe UserEntity.UnsupportedDeleteCommand(s"Cannot execute ${UserEntity.DeleteUserCommand.getClass.getSimpleName}, user in ${UserEntity.DeletedState.getClass.getSimpleName} !")

          result4.hasNoEvents shouldBe true
        }
      }
    }
  }
}