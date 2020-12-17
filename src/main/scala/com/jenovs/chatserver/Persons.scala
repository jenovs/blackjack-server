package com.jenovs.chatserver

import Deck._
import io.circe.Encoder
import io.circe.syntax._

object Persons {
  type Hand = List[Card]

  case class User(id: String, displayName: String, balance: Long) {}

  case class Player(userId: String, displayName: String, balance: Long, hand: Hand, bet: Long) {
    def score = getScore()._1

    def getScore() = {
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

      if (sumMax == sumMin) (sumMin.toString, isBust)
      else if (sumMax <= 21) (s"$sumMin/$sumMax", false)
      else (sumMin.toString, isBust)
    }

    val isBust = getScore()._2
  }

  object Player {
    implicit val PlayerEncoder = Encoder[String]
      .contramap[Player](
        player => {
          Map(
            "displayName" -> player.displayName.toString,
            "hand"        -> player.hand.asJson.noSpaces,
            "balance"     -> player.balance.toString,
            "bet"         -> player.bet.toString,
            "score"       -> player.score,
            "isBust"      -> (if (player.isBust) "1" else "")
          ).asJson.noSpaces
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

      if (sumMax == sumMin) (sumMin.toString, isBust)
      else if (sumMax <= 21) (s"$sumMin/$sumMax", false)
      else (sumMin.toString, isBust)
    }
  }
}
