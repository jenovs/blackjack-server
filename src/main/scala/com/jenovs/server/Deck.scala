package com.jenovs.server

import io.circe._
import io.circe.generic.semiauto._
import io.circe.generic.extras._
// import io.circe.Decoder.Result
// import io.circe.generic.JsonCodec
// import io.circe.parser._
import io.circe.syntax._

import scala.util.Random
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object Deck {
  sealed abstract class Suit(val value: Char)

  object Suit {
    case object Spade    extends Suit('s')
    case object Heart    extends Suit('h')
    case object Club     extends Suit('c')
    case object Diamond  extends Suit('d')
    case object HoleSuit extends Suit('-')

    val Values: Set[Suit]                   = Set(Spade, Heart, Club, Diamond)
    private val ByValue: Map[Char, Suit]    = Values.map(suit => suit.value -> suit).toMap
    def get(value: Char): Option[Suit]      = ByValue.get(value)
    implicit val SuitEncoder: Encoder[Suit] = deriveEncoder[Suit]
  }

  sealed abstract class Rank(val value: Char)
  case object Two      extends Rank('2')
  case object Three    extends Rank('3')
  case object Four     extends Rank('4')
  case object Five     extends Rank('5')
  case object Six      extends Rank('6')
  case object Seven    extends Rank('7')
  case object Eight    extends Rank('8')
  case object Nine     extends Rank('9')
  case object Ten      extends Rank('T')
  case object Jack     extends Rank('J')
  case object Queen    extends Rank('Q')
  case object King     extends Rank('K')
  case object Ace      extends Rank('A')
  case object HoleRank extends Rank('0')

  final case class Card(rank: Rank, suit: Suit)
  case class CardJson(rank: Char, suit: Char, score: Int)
  implicit val CardJsonEncoder: Encoder[CardJson] = deriveEncoder[CardJson]

  object Card {
    implicit val CardEncoder: Encoder[Card] = Encoder[CardJson].contramap[Card](card => {
      val score = card.rank match {
        case Ten | Jack | Queen | King => 10
        case Ace                       => 11
        case rank                      => rank.value.asDigit
      }

      CardJson(card.rank.value, card.suit.value, score)
    })
  }

  val suits = Suit.Values
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

  val holeCard = Card(HoleRank, Suit.HoleSuit)

  case class Deck(
      pCards: List[Card] = for (r <- ranks; s <- suits) yield Card(r, s)
  ) {
    val cards =
      if (isValidDeck(pCards)) pCards
      else throw new RuntimeException("Deck is invalid!!!")

    def shuffle() = new Deck(Random.shuffle(cards))
    // def shuffle() = new Deck(cards)

    def takeOne() = Try(cards.head, new Deck(cards.tail)) match {
      case Failure(e)     => None
      case Success(value) => Some(value)
    }

    def length() = pCards.length

    private def isValidDeck(cards: List[Card]) =
      cards.size <= 52 && cards.distinct.size == cards.size

  }

}
