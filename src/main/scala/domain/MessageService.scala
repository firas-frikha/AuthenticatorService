package domain

trait MessageService {
  def sendEmail(receiver: String): Unit
}
