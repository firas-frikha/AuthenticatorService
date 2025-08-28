package application

import akka.Done

import scala.concurrent.Future

trait UserEntityRepository {

  def insertUser(user: UserView): Future[Done]

  def deleteUser(userId: String): Future[Done]

  def updateUserPassword(passwordHash: String): Future[Done]

  def updateUserVerification(userId: String): Future[Done]
}
