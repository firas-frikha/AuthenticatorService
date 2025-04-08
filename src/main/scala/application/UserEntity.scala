package application

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
}