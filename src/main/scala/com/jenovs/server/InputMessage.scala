package com.jenovs.server

import scala.util.Try

/*
 * Trait for any input operation that could come from the user
 */
sealed trait InputMessage {
  val user: String
}

case class JoinTable(user: String, tableNumber: Int, displayName: String) extends InputMessage
//
case class Bet(user: String, amount: Long) extends InputMessage
case class Hit(user: String)               extends InputMessage
case class Stand(user: String)             extends InputMessage
case class Split(user: String)             extends InputMessage
//
case class Chat(user: String, text: String)           extends InputMessage
case class EnterRoom(user: String, room: String)      extends InputMessage
case class Disconnect(user: String)                   extends InputMessage
case class UnknownCommand(user: String, text: String) extends InputMessage
case class Check(user: String = "")                   extends InputMessage

object InputMessage {
  val DefaultRoomName = "default"

  // Parses a string into a command
  def parse(user: String, text: String): InputMessage =
    splitFirstTwoWords(text) match {
      case ("#join", seatNumber, displayName) => {
        val _tableNumber = Try(seatNumber.trim.toInt).getOrElse(0)

        JoinTable(user, _tableNumber, displayName)
      }
      case ("#bet", amount, _) => Bet(user, Try(amount.trim.toLong).getOrElse(0))
      case ("#hit", _, _)      => Hit(user)
      case ("#stand", _, _)    => Stand(user)
      case ("#split", _, _)    => Split(user)
      case (s"#$cmd", _, _)    => UnknownCommand(user, s"#info Unknown command - $cmd")
      case _                   => Chat(user, text)
    }

  private def splitFirstWord(text: String): (String, String) = {
    val trimmedText = text.trim
    val firstSpace  = trimmedText.indexOf(' ')
    if (firstSpace < 0)
      (trimmedText, "")
    else
      (trimmedText.substring(0, firstSpace), trimmedText.substring(firstSpace + 1).trim)
  }

  private def splitFirstTwoWords(text: String): (String, String, String) = {
    val (first, intermediate) = splitFirstWord(text)
    val (second, rest)        = splitFirstWord(intermediate)

    (first, second, rest)
  }
}
