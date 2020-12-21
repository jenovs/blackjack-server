package com.jenovs.server

import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._

import Deck._
import HandStatus._
import com.jenovs.server.HandStatus.HandStatus.Active0
import com.jenovs.server.HandStatus.HandStatus.Active1
import io.circe.generic.JsonCodec
import com.jenovs.server.Persons.Player.Action

object Persons {
  type Hand = List[Card]
  object Hand {
    def getScore(hand: Hand) = {
      val aces = hand.filter(card => card.rank == Ace).size
      val sumMin = hand
        .map(
          card =>
            card.rank match {
              case Ten | Jack | Queen | King => 10
              case Ace                       => 1
              case rank                      => rank.value.asDigit
            }
        )
        .sum

      val sumMax = if (aces > 0) sumMin + 10 else sumMin

      val isBust = sumMin > 21

      if (hand.size == 2 && sumMax == 21) ("21", false)
      else if (sumMax == sumMin) (sumMin.toString, isBust)
      else if (sumMax <= 21) (s"$sumMin/$sumMax", false)
      else (sumMin.toString, isBust)
    }
  }

  case class PlayerHand(cards: Hand, status: HandStatus = HandStatus.Active) {
    val score  = Hand.getScore(cards)._1
    val isBust = Hand.getScore(cards)._2
  }
  case class PlayerHandJson(cards: Hand, score: String, isBust: Boolean, status: HandStatus)
  // implicit val PlayerHandEncoder: Encoder[PlayerHand] = deriveEncoder[PlayerHand]
  implicit val PlayerHandJsonEncoder: Encoder[PlayerHandJson] = deriveEncoder[PlayerHandJson]
  implicit val PlayerHandEncoder: Encoder[PlayerHand] =
    Encoder[PlayerHandJson]
      .contramap[PlayerHand](
        h => {
          // val activeHand = if (p.handStatus == Active0) 0 else if (p.handStatus == Active1) 1 else -1
          PlayerHandJson(h.cards, h.score, h.isBust, h.status)
        }
      )

  case class User(id: String, displayName: String, balance: Long) {}

  case class Player(
      userId: String,
      displayName: String,
      balance: Long,
      hands: List[PlayerHand],
      bet: Long,
      handStatus: HandStatus,
      activeHand: Int,
      actions: List[Action],
      isActive: Boolean
  )

  object Player {
    case class PlayerJson(
        displayName: String,
        // hand: Map[String, List[Card]],
        // hands: List[Map[String, Hand]],
        hands: List[PlayerHand],
        balance: Long,
        bet: Long,
        activeHand: Int,
        actions: List[Action],
        isActive: Boolean
        // score: String,
        // isBust: Boolean
    )

    implicit val PlayerJsonEncoder: Encoder[PlayerJson] = deriveEncoder[PlayerJson]
    implicit val PlayerEncoder: Encoder[Player] =
      Encoder[PlayerJson]
        .contramap[Player](
          p => {
            // val activeHand = if (p.handStatus == Active0) 0 else if (p.handStatus == Active1) 1 else -1
            PlayerJson(
              p.displayName,
              p.hands,
              p.balance,
              p.bet,
              p.activeHand,
              p.actions,
              p.isActive
            )
          }
        )

    def getScore(hand: Hand) = {
      val aces = hand.filter(card => card.rank == Ace).size
      val sumMin = hand
        .map(
          card =>
            card.rank match {
              case Ten | Jack | Queen | King => 10
              case Ace                       => 1
              case rank                      => rank.value.asDigit
            }
        )
        .sum

      val sumMax = if (aces > 0) sumMin + 10 else sumMin

      val isBust = sumMin > 21

      if (hand.size == 2 && sumMax == 21) ("21", false)
      else if (sumMax == sumMin) (sumMin.toString, isBust)
      else if (sumMax <= 21) (s"$sumMin/$sumMax", false)
      else (sumMin.toString, isBust)
    }

    trait Action
    object Action {
      implicit val ActionEncoder: Encoder[Action] = Encoder[String].contramap[Action](_.toString)

      case object Bet   extends Action
      case object Hit   extends Action
      case object Split extends Action
      case object Stand extends Action
    }
  }
}
