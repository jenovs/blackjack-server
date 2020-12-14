package controllers

import io.circe.Codec
import io.circe.generic.semiauto._
import io.circe.generic.extras._
import io.circe.Decoder
import io.circe.Encoder
import io.circe._
import io.circe.Decoder.Result
import io.circe.generic.JsonCodec
import io.circe.parser._
import io.circe.syntax._

import scala.util.Random
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object Deck {
  sealed abstract class Suit
  case object Spade extends Suit {
    override def toString = "s"
  }
  case object Heart extends Suit {
    override def toString = "h"
  }
  case object Club extends Suit {
    override def toString = "c"
  }
  case object Diamond extends Suit {
    override def toString = "d"
  }

  sealed abstract class Rank
  case object Two extends Rank {
    override def toString = "2"
  }
  case object Three extends Rank {
    override def toString = "3"
  }
  case object Four extends Rank {
    override def toString = "4"
  }
  case object Five extends Rank {
    override def toString = "5"
  }
  case object Six extends Rank {
    override def toString = "6"
  }
  case object Seven extends Rank {
    override def toString = "7"
  }
  case object Eight extends Rank {
    override def toString = "8"
  }
  case object Nine extends Rank {
    override def toString = "9"
  }
  case object Ten extends Rank {
    override def toString = "T"
  }
  case object Jack extends Rank {
    override def toString = "J"
  }
  case object Queen extends Rank {
    override def toString = "Q"
  }
  case object King extends Rank {
    override def toString = "K"
  }
  case object Ace extends Rank {
    override def toString = "A"
  }

  final case class Card(rank: Rank, suit: Suit)
  object Card {
    implicit val rankCodec = deriveCodec[Rank]
    // implicit val rankEncoder: Encoder[Card] = Encoder.forProduct1("rank")(a => (a.rank))
    implicit val suitCodec = deriveCodec[Suit]
    implicit val cardEncoder: Encoder[Card] =
      Encoder.forProduct3("rank", "suit", "value")(a =>
        (
          a.rank.toString,
          a.suit.toString,
          a.rank match {
            case Ten | Jack | Queen | King => 10
            case Ace                       => 11
            case n                         => n.toString.toInt
          }
        )
      )
    // implicit val cardCodec = deriveCodec[Card]

  }

  val suits = Set(Spade, Heart, Club, Diamond)
  val ranks = List(
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace
  )

  // case class Card(rank: Rank, suit: Suit)
  // implicit val cardCodec = deriveCodec[Card]

  case class Deck(
      pCards: List[Card] = for (r <- ranks; s <- suits) yield Card(r, s)
  ) {

    val cards =
      if (isValidDeck(pCards)) pCards
      else throw new RuntimeException("Deck is invalid!!!")

    def shuffle() = new Deck(Random.shuffle(cards))

    def takeOne() = Try(cards.head, new Deck(cards.tail)) match {
      case Failure(e)     => None
      case Success(value) => Some(value)
    }

    def length() = pCards.length

    private def isValidDeck(cards: List[Card]) =
      cards.size <= 52 && cards.distinct.size == cards.size

  }
}
