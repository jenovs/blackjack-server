package com.jenovs.server

import io.circe.syntax._
import java.time.Instant

import ChatState.Playing
import Persons.{Player, PlayerHand}
import HandStatus.HandStatus._
import Helpers.getSeatAndPlayer

object HitHandler {
  def handleHit(userId: String, state: ChatState) = {
    val deck           = state.deck
    val orderOfPlayers = state.orderOfPlayers
    val status         = state.status
    val table          = state.table

    val (seat, player) = getSeatAndPlayer(userId, table).get

    if (status != Playing) (state, Seq(SendToUser(userId, s"#info Game is not running.")))
    else if (orderOfPlayers.headOption.getOrElse(0) != seat)
      (state, Seq(SendToUser(userId, s"#info Not your turn.")))
    else {
      val (card, nextDeck) = deck.takeOne().get

      val hand0 = player.hands.lift(0)
      val hand1 = player.hands.lift(1)

      val playerHand      = player.hands(player.activeHand)
      val playerCards     = playerHand.cards
      val nextCards       = playerCards :+ card
      val (score, isBust) = Player.getScore(nextCards)
      val nextBet         = if (isBust) 0 else player.bet
      val is21            = score == "21" || score.split("/").last == 21
      val nextHandStatus  = if (isBust) Bust else if (is21) Standing else playerHand.status
      val nextHand        = PlayerHand(nextCards, nextHandStatus)

      val nextHands =
        if (player.hands.length == 1) List(nextHand)
        else {
          player.activeHand match {
            case 0 => List(nextHand, player.hands(1))
            case 1 => List(player.hands(0), nextHand)
          }
        }

      val isHandDone = nextHandStatus == Bust || nextHandStatus == Standing

      val nextActiveHand =
        if (!isHandDone) player.activeHand
        else if (isHandDone && player.hands.length == 2 && player.activeHand == 0) 1
        else -1

      val nextPlayer =
        player.copy(
          activeHand = nextActiveHand,
          hands = nextHands,
          bet = nextBet,
          handStatus = nextHandStatus
        )
      val _nextTable = table + (seat -> nextPlayer)
      // val hasNextMove        = nextHandStatus != Bust && nextHandStatus != Standing
      val hasNextMove =
        if (player.hands.length == 2 && player.activeHand == 0) true
        else if (player.hands.length == 1 || player.activeHand == 1)
          nextHandStatus != Bust && nextHandStatus != Standing
        else true

      val nextOrderOfPlayers = if (!hasNextMove) orderOfPlayers.drop(1) else orderOfPlayers
      val nextActivePlayer   = if (nextOrderOfPlayers.length > 0) nextOrderOfPlayers.head else 0

      val lastActionAt = Instant.now().toEpochMilli / 1000

      val nextTable = _nextTable.map {
        case (seat, player) => (seat -> player.copy(isActive = nextActivePlayer == seat))
      }

      (
        state.copy(
          table = nextTable,
          deck = nextDeck,
          orderOfPlayers = nextOrderOfPlayers,
          lastActionAt = lastActionAt
        ),
        Seq(SendToAll(nextTable.asJson.noSpaces))
      )
    }
  }
}
