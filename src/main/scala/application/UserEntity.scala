package application

import akka.actor.typed.Behavior
import akka.persistence.typed.PersistenceId
import akka.persistence.typed.scaladsl.EventSourcedBehavior
import akka.persistence.typed.scaladsl.ReplyEffect

object UserEntity {

  sealed trait State {
    def id: String

    def applyCommand(command: Command): ReplyEffect[Event, State]

    def applyEvent(event: Event): State
  }

  sealed trait Command

  sealed trait Event

  case class EmptyState(override val id: String) extends State {

    override def applyCommand(command: Command): ReplyEffect[Event, State] = ???

    override def applyEvent(event: Event): State = ???
  }

  case class PendingVerificationState(override val id: String) extends State {

    override def applyCommand(command: Command): ReplyEffect[Event, State] = ???

    override def applyEvent(event: Event): State = ???
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