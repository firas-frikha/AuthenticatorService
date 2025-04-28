package application

import akka.actor.typed.{ActorRef, Behavior}
import akka.persistence.typed.PersistenceId
import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior, ReplyEffect}

object UserEntity {

  sealed trait State {
    def id: String

    def applyCommand(command: Command): ReplyEffect[Event, State]

    def applyEvent(event: Event): State
  }

  sealed trait Command

  case class RegisterUserCommand(firstName: String,
                                 lastName: String,
                                 userId: String,
                                 password: String)
                                (val replyTo: ActorRef[RegisterCommandResult]) extends Command

  sealed trait Event {
    def id: String
  }

  case class RegisteredUserEvent(id: String,
                                 firstName: String,
                                 lastName: String,
                                 userId: String,
                                 password: String) extends Event


  sealed trait Result

  sealed trait RegisterCommandResult extends Result

  case object SuccessfulRegisterCommandUserCommand extends RegisterCommandResult

  case class UnsupportedRegisterCommandUserCommand(reason: String) extends RegisterCommandResult


  case class EmptyState(override val id: String) extends State {

    override def applyCommand(command: Command): ReplyEffect[Event, State] =
      command match {
        case registerCommand: RegisterUserCommand =>
          Effect
            .persist(RegisteredUserEvent(
              id = registerCommand.userId,
              firstName = registerCommand.firstName,
              lastName = registerCommand.lastName,
              userId = registerCommand.userId,
              password = registerCommand.password
            ))
            .thenReply(registerCommand.replyTo)(_ => SuccessfulRegisterCommandUserCommand)
      }

    override def applyEvent(event: Event): State =
      event match {
        case registeredUserEvent: RegisteredUserEvent =>
          PendingVerificationState(
            id = registeredUserEvent.id,
            firstName = registeredUserEvent.firstName,
            lastName = registeredUserEvent.lastName,
            userId = registeredUserEvent.userId,
            password = registeredUserEvent.password)
      }
  }

  case class PendingVerificationState(override val id: String,
                                      firstName: String,
                                      lastName: String,
                                      userId: String,
                                      password: String) extends State {

    override def applyCommand(command: Command): ReplyEffect[Event, State] =
      command match {
        case registerUSerCommand: RegisterUserCommand =>
          Effect.reply(registerUSerCommand.replyTo)(UnsupportedRegisterCommandUserCommand("Unable to register user, user already registered"))
      }

    override def applyEvent(event: Event): State =
      event match {
        case registeredUserEvent: RegisteredUserEvent =>
          throw new IllegalStateException(s"Unexpected event ${registeredUserEvent.getClass.getName}")
      }
  }

  case class ActiveState(override val id: String) extends State {

    override def applyCommand(command: Command): ReplyEffect[Event, State] = ???

    override def applyEvent(event: Event): State = ???
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