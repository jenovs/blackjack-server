package com.jenovs.server

import io.circe.syntax._
import java.time.Instant

import ChatState.Playing
import Persons.{Player, PlayerHand}
import HandStatus.HandStatus._
import Helpers.getSeatAndPlayer

object StandHandler {
  def handleStand(userId: String, state: ChatState) = {
    val deck           = state.deck
    val orderOfPlayers = state.orderOfPlayers
    val status         = state.status
    val table          = state.table

    val (seat, player) = getSeatAndPlayer(userId, table).get

    if (status != Playing) (state, Seq(SendToUser(userId, s"#info Game is not running.")))
    else if (orderOfPlayers.headOption.getOrElse(0) != seat)
      (state, Seq(SendToUser(userId, s"#info Not your turn.")))
    else {
      val lastActionAt = Instant.now().toEpochMilli / 1000

      if (player.hands.length == 1 || (player.hands.length == 2 && player.activeHand == 1)) {
        val nextOrderOfPlayers = orderOfPlayers.drop(1)
        val nextPlayer         = player.copy(activeHand = -1)
        val _nextTable         = table + (seat -> nextPlayer)
        val nextActivePlayer   = if (nextOrderOfPlayers.length > 0) nextOrderOfPlayers.head else 0

        val nextTable = _nextTable.map {
          case (seat, player) => (seat -> player.copy(isActive = nextActivePlayer == seat))
        }

        (
          state.copy(table = nextTable, orderOfPlayers = nextOrderOfPlayers),
          Seq(SendToAll(nextTable.asJson.noSpaces))
        )
      } else {
        val nextPlayer = player.copy(activeHand = 1)
        val nextTable  = table + (seat -> nextPlayer)
        (
          state.copy(table = nextTable, lastActionAt = lastActionAt),
          Seq(SendToAll(nextTable.asJson.noSpaces))
        )
      }
    }
  }
}
