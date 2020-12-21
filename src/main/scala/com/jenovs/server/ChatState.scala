package com.jenovs.server

import io.circe.syntax._
import scala.concurrent.duration._

import Deck._
import Persons._
import HandStatus.HandStatus._
import com.jenovs.server.ChatState._
import com.jenovs.server.ChatState.DealerReveals
import com.jenovs.server.ChatState.Emit
import com.jenovs.server.ChatState.EmitNo
import com.jenovs.server.ChatState.EmitYes
import cats.effect.IO
import com.jenovs.server.Persons.Player.Action
import java.time.Instant

import HitHandler.handleHit
import StandHandler.handleStand

object ChatState {
  trait GameStatus
  case object Idle          extends GameStatus
  case object Betting       extends GameStatus
  case object Dealing       extends GameStatus
  case object Playing       extends GameStatus
  case object DealerReveals extends GameStatus
  case object DealerDraws   extends GameStatus
  case object Showdown      extends GameStatus
  case object Restart       extends GameStatus

  trait Emit
  case object EmitYes extends Emit
  case object EmitNo  extends Emit

  val emptyHand = List(PlayerHand(List[Card]()))

  val dealer = Player("", "Dealer", 1, emptyHand, 1, Active0, 0, List(), false)
  // Default constructor
  def apply(): ChatState =
    ChatState(
      userRooms = Map.empty,
      roomMembers = Map.empty,
      //
      users = Map.empty,
      table = Map(0 -> dealer),
      deck = Deck().shuffle(),
      dealerHand = List[Card](),
      status = Idle,
      // players = Map.empty,
      orderOfPlayers = List[Int](),
      bettingEndsAt = 0,
      lastActionAt = 0
    )
}

case class ChatState(
    userRooms: Map[String, String],
    roomMembers: Map[String, Set[String]],
    //
    users: Map[String, User],
    table: Map[Int, Player],
    deck: Deck,
    // users: List[String],
    dealerHand: Hand,
    status: com.jenovs.server.ChatState.GameStatus,
    // players: Map[String, Player],
    orderOfPlayers: List[Int],
    bettingEndsAt: Long,
    lastActionAt: Long
) {
  def getTableJson() = {
    this.table.asJson.noSpaces
  }

  def getCards(count: Int, deck: Deck, cards: List[Card] = List[Card]()): (List[Card], Deck) = {
    if (count < 1) (cards, deck)
    else {
      val (card, nextDeck) = deck.takeOne().get
      getCards(count - 1, nextDeck, cards :+ card)
    }
  }

  def check(): (ChatState, Boolean, Emit) = {
    val update   = true
    val noUpdate = false

    status match {
      case Betting => {
        val hasAnyoneBet = this.table.drop(1).find { case (_, player) => player.bet > 0 } match {
          case None        => false
          case Some(value) => true
        }

        hasAnyoneBet match {
          case true => {
            val currentTime = Instant.now().toEpochMilli / 1000

            val nextStatus =
              if (this.bettingEndsAt > 0 && bettingEndsAt - currentTime <= 0) Dealing else Betting
            // val endOfBetting = Instant.now().toEpochMilli / 1000 + 5.seconds.toSeconds

            (this.copy(status = nextStatus), true, EmitNo)
          }
          case false => (this, false, EmitNo)
        }
      }
      case Dealing => {
        val count             = table.filter { case (_, player) => player.bet > 0 }.keys.size
        val (cards, nextDeck) = getCards(count * 2, Deck().shuffle())

        val dealerHand    = cards.slice(0, 2)
        val activePlayers = table.filter { case (_, player) => player.bet > 0 }.keys.toList.sorted

        val nextTable = (for {
          (key, i) <- activePlayers.zipWithIndex
          value = i match {
            case 0 =>
              (0 -> table(0).copy(
                hands = List[PlayerHand](
                  PlayerHand(List[Card](dealerHand(0), holeCard))
                )
              ))
            case _ => {
              if (table(key).bet <= 0) (key -> table(key).copy(actions = List()))
              else {
                val hand           = cards.slice(i * 2, i * 2 + 2)
                val (score, _)     = Player.getScore(hand)
                val canSplit       = hand.map(_.rank).toSet.size == 1
                val defaultActions = List[Action](Action.Hit, Action.Stand)
                val actions        = if (canSplit) defaultActions :+ Action.Split else defaultActions

                (key -> table(key).copy(
                  hands = List(PlayerHand(hand)),
                  handStatus =
                    if (score == "21") Blackjack
                    else Active0,
                  activeHand = 0,
                  actions = actions
                ))
              }
            }
          }
        } yield value).toMap

        val nextOrderOfPlayers =
          nextTable.keys.toList.sorted
            .drop(1)
            .filter(seat => nextTable(seat).bet > 0 && nextTable(seat).handStatus != Blackjack)

        val nextActivePlayer = if (nextOrderOfPlayers.length > 0) nextOrderOfPlayers.head else 0
        val lastActionAt     = Instant.now().toEpochMilli / 1000

        val inactivePlayersKeys =
          table.filter { case (_, player) => player.bet <= 0 }.keys.toList.sorted
        val inactivePlayers = for {
          key <- inactivePlayersKeys
        } yield (key -> table(key).copy(actions = List()))

        (
          this.copy(
            deck = nextDeck,
            table = nextTable.map {
              case (seat, player) => (seat -> player.copy(isActive = nextActivePlayer == seat))
            }
              ++ inactivePlayers.toMap,
            orderOfPlayers = nextOrderOfPlayers,
            dealerHand = dealerHand,
            status = Playing,
            lastActionAt = lastActionAt
          ),
          true,
          EmitYes
        )
      }
      case Playing => {
        if (orderOfPlayers.isEmpty) {
          (this.copy(status = DealerReveals), true, EmitNo)
        } else if (Instant.now().toEpochMilli / 1000 - lastActionAt > 5) {
          val nextOrderOfPlayers = orderOfPlayers.drop(1)
          val nextActivePlayer   = if (nextOrderOfPlayers.length > 0) nextOrderOfPlayers.head else 0

          val lastActionAt = Instant.now().toEpochMilli / 1000

          val nextTable = table.map {
            case (seat, player) => (seat -> player.copy(isActive = nextActivePlayer == seat))
          }
          (
            this.copy(table = nextTable, lastActionAt = lastActionAt, orderOfPlayers = nextOrderOfPlayers),
            true,
            EmitYes
          )
        } else (this, false, EmitNo)
      }
      case DealerReveals => {
        val standingPlayers = table.filter {
          case (seat, player) => seat != 0 && !player.hands.forall(_.isBust)
        }
        if (standingPlayers.isEmpty) {
          // reset game
          val nextTable = for {
            (seat, player) <- table
            nextPlayer = player.copy(
              hands = List(PlayerHand(List[Card]())),
              activeHand = -1,
              actions = List(Action.Bet)
            )
          } yield (seat -> nextPlayer)
          (this.copy(table = nextTable, status = Betting), true, EmitYes)
        } else {
          val nextDealer =
            table(0).copy(hands = List(PlayerHand(dealerHand)))
          val nextTable = table + (0 -> nextDealer)
          (this.copy(table = nextTable, status = DealerDraws), true, EmitYes)
        }
      }
      case DealerDraws => {
        val dealer = table(0)
        val score  = dealer.hands(0).score.split("/").map(_.toInt).max
        if (score < 17) {
          val (card, nextDeck) = deck.takeOne().get
          val nextHand         = dealer.hands(0).cards :+ card
          val (score, isBust)  = Player.getScore(nextHand)
          val nextDealer       = table(0).copy(hands = List(PlayerHand(nextHand)))
          val nextTable        = table + (0 -> nextDealer)
          (this.copy(table = nextTable, deck = nextDeck, status = DealerDraws), true, EmitYes)
        } else {
          (this.copy(status = Showdown), true, EmitNo)
        }
      }
      case Showdown => {
        val dealerScore = table(0).hands(0).score.split("/").takeRight(1)(0).toInt
        val dealer      = table(0)

        val nextTable = for {
          (seat, player) <- table
          if (seat > 0)
          nextHands = player.hands.map(hand => {
            val score              = hand.score.split("/").takeRight(1)(0).toInt
            val hasBlackjack       = hand.cards.size == 2 && hand.score == "21"
            val hasDealerBlackjack = dealer.hands(0).cards.size == 2 && dealer.hands(0).score == "21"
            hand.copy(
              status =
                if (score > 21) Bust
                else if (hasBlackjack && !hasDealerBlackjack) Blackjack
                else if (hasDealerBlackjack && !hasBlackjack)
                  Lost
                else if (score > dealerScore || dealerScore > 21) Won
                else if (score < dealerScore && dealerScore <= 21) Lost
                else Push
            )
          })
        } yield (seat -> player.copy(
          hands = nextHands,
          balance = player.balance + nextHands
            .map(hand => {
              val status = hand.status
              if (status == Blackjack) player.bet * 2.5
              else if (status == Won) player.bet * 2
              else if (status == Push) player.bet
              else 0
            })
            .sum
            .toLong
        ))

        (this.copy(table = table + (0 -> table(0)) ++ nextTable, status = Restart), true, EmitYes)
      }
      case Restart => {
        Thread.sleep(2000)
        val nextTable = for {
          (seat, player) <- table
        } yield (seat -> player.copy(
          hands = emptyHand,
          bet = if (seat == 0) 1 else 0,
          actions = List(Action.Bet)
        ))

        (
          this.copy(
            deck = Deck().shuffle(),
            table = nextTable,
            dealerHand = List[Card](),
            orderOfPlayers = List[Int](),
            status = Betting,
            bettingEndsAt = 0
          ),
          true,
          EmitYes
        )
      }
      case _ => (this, false, EmitNo)
    }
  }

  def process(msg: InputMessage): (ChatState, Seq[OutputMessage]) = msg match {
    case JoinTable(userId, seatNumber, displayName) => {
      val user = users.get(userId)

      val nextUsers = user match {
        case None        => users + (userId -> User(userId, displayName, 100))
        case Some(value) => users
      }

      val tableSeat = table.get(seatNumber) //.getOrElse("")

      val isSeated = table.values.find(player => player.userId == userId) match {
        case None        => false
        case Some(value) => true
      }

      val nextState = this.copy(users = nextUsers)

      if (isSeated) {
        (nextState, Seq(SendToUser(userId, s"#info You're already at the table")))
      } else if (seatNumber < 1 || seatNumber > 5) {
        (nextState, Seq(SendToUser(userId, s"#info Seat $seatNumber is invalid")))
      } else if (tableSeat.nonEmpty) {
        (nextState, Seq(SendToUser(userId, s"#info Seat $seatNumber is occupied")))
      } else {
        val balance = nextUsers(userId).balance
        // val nextTable = table + (seatNumber -> Player(userId, displayName, balance, List[Card](), 0))
        val nextTable = table + (seatNumber -> Player(
          userId,
          displayName,
          balance,
          emptyHand,
          0,
          Active0,
          -1,
          List(Action.Bet),
          false
        ))

        val nextStatus = status match {
          case Idle => Betting
          case s    => s
        }

        (
          nextState.copy(table = nextTable, status = nextStatus),
          Seq(SendToAll(nextTable.asJson.noSpaces))
        )
      }

    }

    case Bet(userId, amount) => {
      val (seat, player) = table.find { case (seat, player) => player.userId == userId }.get

      if (player.balance < amount) (this, Seq(SendToUser(userId, "#info Not enough funds.")))
      else if (status != Betting) (this, Seq(SendToUser(userId, "#info Betting is not allowed.")))
      else {
        val nextPlayer = player.copy(balance = player.balance - amount, bet = amount)
        val nextTable  = table + (seat -> nextPlayer)

        val hasAllBetted = nextTable.forall { case (_, player) => player.bet > 0L }

        val nextStatus = if (hasAllBetted) Dealing else Betting

        val nextBettingEndsAt =
          if (hasAllBetted) 0
          else if (bettingEndsAt > 0) bettingEndsAt
          else Instant.now().toEpochMilli / 1000 + 5.seconds.toSeconds

        val nextOrderOfPlayers = if (hasAllBetted) table.keys.toList.sorted.drop(1) else orderOfPlayers

        (
          this.copy(
            table = nextTable,
            status = nextStatus,
            orderOfPlayers = nextOrderOfPlayers,
            bettingEndsAt = nextBettingEndsAt
          ),
          Seq(SendToAll(nextTable.asJson.noSpaces))
        )
      }
    }

    case Hit(userId) => handleHit(userId, this)

    case Stand(userId) => handleStand(userId, this)

    case Split(userId) => {
      if (status != Playing) (this, Seq(SendToUser(userId, s"#info Game is not running.")))
      else {
        val (seat, player) = table.find {
          case (_, player) => {
            player.userId == userId
          }
        }.get

        if (orderOfPlayers.headOption.getOrElse(0) != seat)
          (this, Seq(SendToUser(userId, s"#info Not your turn.")))
        else if (player.balance < player.bet) (this, Seq(SendToUser(userId, s"#info Balance too low.")))
        else {
          val (cards, nextDeck) = getCards(2, deck)
          val playerCards       = player.hands(0).cards
          val hand0             = PlayerHand(List(playerCards(0), cards(0)))
          val hand1             = PlayerHand(List(playerCards(1), cards(1)))

          val nextHands = List[PlayerHand](hand0, hand1)
          val nextPlayer =
            player.copy(
              hands = nextHands,
              actions = List(Action.Hit, Action.Stand),
              activeHand = 0,
              balance = player.balance - player.bet
            )
          val nextTable = table + (seat -> nextPlayer)

          (this.copy(deck = nextDeck, table = nextTable), Seq(SendToAll(nextTable.asJson.noSpaces)))
        }
      }
    }

    case Chat(user, text) =>
      userRooms.get(user) match {
        case Some(room) =>
          val (card1, _newDeck) = deck.takeOne.get
          val (card2, newDeck)  = _newDeck.takeOne.get
          val nextDealerHand    = dealerHand.appended(card2)
          val nextState         = this.copy(deck = newDeck, dealerHand = nextDealerHand)
          (nextState, sendToRoom(room, s"${nextDealerHand}"))

        case None =>
          (this, Seq(SendToUser(user, "#info You are not currently in a room")))
      }

    case EnterRoom(user, toRoom) =>
      userRooms.get(user) match {
        case None =>
          // First time in - welcome and enter
          // val (finalState, enterMessages) = addToRoom(user, toRoom)

          (this, Seq(SendToUser(user, table.asJson.noSpaces)))

        case Some(currentRoom) if currentRoom == toRoom =>
          (this, Seq(SendToUser(user, "#info You are already in that room!")))

        case Some(_) =>
          // Already in - move from one room to another
          val (intermediateState, leaveMessages) = removeFromCurrentRoom(user)
          val (finalState, enterMessages)        = intermediateState.addToRoom(user, toRoom)

          (finalState, leaveMessages ++ enterMessages)
      }

    case Disconnect(userId) => {
      val nextTable = table.find { case (seat, player) => player.userId == userId } match {
        case None        => table
        case Some(value) => table - value._1
      }

      val hasNoPlayers = nextTable.size == 1

      hasNoPlayers match {
        case true => {
          //
        }
        case false => {
          //
        }
      }

      val nextStatus = if (hasNoPlayers) Idle else status
      val nextDealer =
        if (hasNoPlayers) dealer.copy(hands = emptyHand) else dealer
      val nextDealerHand = if (hasNoPlayers) List[Card]() else dealerHand
      // val nextStatus = if

      (
        this.copy(table = nextTable + (0 -> nextDealer), dealerHand = nextDealerHand, status = nextStatus),
        Seq(SendToAll(nextTable.asJson.noSpaces))
      )
    }

    case Check(user) => {
      (this, Nil)
    }

    case UnknownCommand(user, text) =>
      (this, Seq(SendToUser(user, text)))
  }

  private def sendToRoom(room: String, text: String): Seq[OutputMessage] = {
    roomMembers
      .get(room)
      .map(SendToUsers(_, text))
      .toSeq
  }

  private def removeFromCurrentRoom(user: String): (ChatState, Seq[OutputMessage]) =
    userRooms.get(user) match {
      case Some(room) =>
        val nextMembers = roomMembers.getOrElse(room, Set()) - user
        val nextState =
          if (nextMembers.isEmpty)
            this.copy(userRooms = userRooms - user, roomMembers = roomMembers - room)
          else
            this.copy(userRooms = userRooms - user, roomMembers = roomMembers + (room -> nextMembers))

        // Send to "previous" room population to include the leaving user
        (nextState, sendToRoom(room, s"#info $user has left $room"))
      case None =>
        (this, Nil)
    }

  private def addToRoom(user: String, room: String): (ChatState, Seq[OutputMessage]) = {
    val nextMembers = roomMembers.getOrElse(room, Set()) + user
    // val nextState   = ChatState(userRooms + (user -> room), roomMembers + (room -> nextMembers), deck)
    val nextState =
      this.copy(
        userRooms = userRooms + (user     -> room),
        roomMembers = roomMembers + (room -> nextMembers)
        // users = users.appended(user)
      )

    // Send to "next" room population to include the joining user
    (nextState, nextState.sendToRoom(room, nextState.table.asJson.noSpaces))
  }
}
