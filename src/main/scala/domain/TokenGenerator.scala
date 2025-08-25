package domain

trait TokenGenerator {

  def generateToken(tokenSize: Int = 32): String

  def hashToken(token: String): String

  def matches(token: String, expectedHashB64: String): Boolean
}
