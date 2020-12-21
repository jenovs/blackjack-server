package com.jenovs.server

object Helpers {
  def getSeatAndPlayer(userId: String, table: Map[Int, Persons.Player]) = {
    table.find {
      case (_, player) => {
        player.userId == userId
      }
    }
  }
}
