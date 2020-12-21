package com.jenovs.server

/*
 * Trait for any message that can be sent downstream to one or more users
 */
trait OutputMessage {
  // To support stream filtering
  def forUser(targetUser: String): Boolean
  def toString: String
}

case class WelcomeUser(user: String) extends OutputMessage {
  override def forUser(targetUser: String): Boolean = targetUser == user
  override def toString: String =
    s"#info Welcome to Blackjack table"
}

case class SendToUser(user: String, text: String) extends OutputMessage {
  override def forUser(targetUser: String): Boolean = targetUser == user
  override def toString: String                     = text
}

case class SendToUsers(users: Set[String], text: String) extends OutputMessage {
  override def forUser(targetUser: String): Boolean = users.contains(targetUser)
  override def toString: String                     = text
}

case class SendToAll(text: String) extends OutputMessage {
  override def forUser(targetUser: String): Boolean = true
  override def toString: String                     = text
}
