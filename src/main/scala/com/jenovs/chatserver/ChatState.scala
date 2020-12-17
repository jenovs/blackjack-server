package com.jenovs.chatserver

import io.circe.syntax._

import Deck._
import Persons._
import com.jenovs.chatserver.ChatState.Betting
import com.jenovs.chatserver.ChatState.Playing
import com.jenovs.chatserver.ChatState.Dealing
import com.jenovs.chatserver.ChatState.Idle
import com.jenovs.chatserver.ChatState.DealerDraws
import cats.instances.order
import com.jenovs.chatserver.ChatState.DealerReveals

object ChatState {
  trait GameStatus
  case object Idle          extends GameStatus
  case object Betting       extends GameStatus
  case object Dealing       extends GameStatus
  case object Playing       extends GameStatus
  case object DealerReveals extends GameStatus
  case object DealerDraws   extends GameStatus

  val emptyHand = List[Card]()

  val dealer = Player("", "Dealer", 1, emptyHand, 1)
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
      players = Map.empty,
      orderOfPlayers = List[Int]()
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
    status: com.jenovs.chatserver.ChatState.GameStatus,
    players: Map[String, Player],
    orderOfPlayers: List[Int]
) {
  def getTableJson() = {
    this.table.asJson.noSpaces
  }

  def checkStatus() = {
    this.status
  }

  def getCards(count: Int, deck: Deck, cards: List[Card] = List[Card]()): (List[Card], Deck) = {
    if (count < 1) (cards, deck)
    else {
      val (card, nextDeck) = deck.takeOne().get
      getCards(count - 1, nextDeck, cards :+ card)
    }
    // val (card, nextDeck) = deck.takeOne()
    // this = this.copy(deck = nextDeck)
  }

  def check() = {
    println(status, orderOfPlayers)
    if (status == Dealing) {
      val count             = table.keys.size
      val (cards, nextDeck) = getCards(count * 2, deck)

      val dealerHand = cards.slice(0, 2)

      val nextTable = (for {
        (key, i) <- table.keys.zipWithIndex
        value = i match {
          case 0 => (0   -> table(0).copy(hand = List[Card](dealerHand(0), holeCard)))
          case _ => (key -> table(key).copy(hand = cards.slice(i * 2, i * 2 + 2)))
        }
      } yield value).toMap

      // table.keys.zipWithIndex.foreach {
      //   case (key, i) => {
      //     nextTable(key) = table(key).copy(hand = cards.slice(i * 2, i * 2 + 2))
      //   }
      // }
      // val keys = table.keys
      // println("keys:", keys)
      // n.asJson.noSpaces
      (this.copy(deck = nextDeck, table = nextTable, dealerHand = dealerHand, status = Playing), true)
    } else if (status == Playing) {
      if (orderOfPlayers.isEmpty) {
        (this.copy(status = DealerReveals), true)
      } else (this, false)
    } else if (status == DealerReveals) {
      println("dealer reveals")
      val standingPlayers = table.filter { case (seat, player) => seat != 0 && !player.isBust }
      println(standingPlayers)
      if (standingPlayers.isEmpty) {
        // reset game
        val nextTable = for {
          (seat, player) <- table
          nextPlayer     = player.copy(hand = List[Card]())
        } yield (seat -> nextPlayer)
        (this.copy(table = nextTable, status = Betting), true)
      } else {
        // dealer reveals
        // val (card, nextDeck) = deck.takeOne().get
        // val (score, isBust) = Player.getScore(dealerHand :+ card)
        // (this.copy(deck = nextDeck), true)
        val nextDealer = table(0).copy(hand = dealerHand)
        println(table)
        val nextTable = table + (0 -> nextDealer)
        (this.copy(table = nextTable, status = DealerDraws), true)
      }
    } else {
      println("check in else")

      (this, false)
    }
  }

  def deal() = {
    println("dealing")

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
      } else if (seatNumber < 1 || seatNumber > 7) {
        (nextState, Seq(SendToUser(userId, s"#info Seat $seatNumber is invalid")))
      } else if (tableSeat.nonEmpty) {
        (nextState, Seq(SendToUser(userId, s"#info Seat $seatNumber is occupied")))
      } else {
        val balance = nextUsers(userId).balance
        // val nextTable = table + (seatNumber -> Player(userId, displayName, balance, List[Card](), 0))
        val nextTable = table + (seatNumber -> Player(userId, displayName, balance, List[Card](), 0))

        val nextStatus = status match {
          case Idle => Betting
          case s    => s
        }

        (
          nextState.copy(table = nextTable, status = nextStatus),
          Seq(SendToAll(s"#info User $displayName joined $seatNumber"))
        )
      }

    }

    case Bet(userId, amount) => {
      if (status != Betting) (this, Seq(SendToUser(userId, "#info Betting is not allowed.")))
      else {
        val (seat, player) = table.find { case (seat, player) => player.userId == userId }.get
        val nextPlayer     = player.copy(balance = player.balance - amount, bet = amount)
        val nextTable      = table + (seat -> nextPlayer)

        val hasAllBetted = nextTable.forall { case (_, player) => player.bet > 0L }

        val nextStatus = if (hasAllBetted) Dealing else Betting

        val nextOrderOfPlayers = if (hasAllBetted) table.keys.toList.sorted.drop(1) else orderOfPlayers

        (
          this.copy(table = nextTable, status = nextStatus, orderOfPlayers = nextOrderOfPlayers),
          Seq(SendToAll(nextTable.asJson.noSpaces))
        )
      }
    }

    case Hit(userId) => {
      val (seat, player) = table.find {
        case (_, player) => {
          player.userId == userId
        }
      }.get

      if (player.isBust) (this, Seq(SendToAll(s"#info Player ${player.displayName} has lost.")))
      else {
        val (card, nextDeck) = deck.takeOne().get

        val (_, isBust) = Player.getScore(player.hand :+ card)

        val nextBet = if (isBust) 0 else player.bet

        val nextPlayer =
          player.copy(hand = (player.hand :+ card), bet = nextBet)
        println(nextPlayer.score)
        val nextTable          = table + (seat -> nextPlayer)
        val nextOrderOfPlayers = if (isBust) orderOfPlayers.drop(1) else orderOfPlayers

        (
          this.copy(table = nextTable, deck = nextDeck, orderOfPlayers = nextOrderOfPlayers),
          Seq(SendToAll(nextTable.asJson.noSpaces))
        )
      }
    }

    case Stand(user) => {
      val nextOrderOfPlayers = orderOfPlayers.drop(1)

      (this.copy(orderOfPlayers = nextOrderOfPlayers), Seq(SendToAll(table.asJson.noSpaces)))
    }

    case Chat(user, text) =>
      println("chatting")
      userRooms.get(user) match {
        case Some(room) =>
          val (card1, _newDeck) = deck.takeOne.get
          val (card2, newDeck)  = _newDeck.takeOne.get
          println(card2)
          println(this.deck.length())
          val nextDealerHand = dealerHand.appended(card2)
          // (this, sendToRoom(room, s"$user: $text"))
          val nextState = this.copy(deck = newDeck, dealerHand = nextDealerHand)
          (nextState, sendToRoom(room, s"${nextDealerHand.asJson.noSpaces}"))
        // (nextState, sendToRoom(room, s"${HoleCard().asJson}"))

        case None =>
          (this, Seq(SendToUser(user, "#info You are not currently in a room")))
      }

    case EnterRoom(user, toRoom) =>
      userRooms.get(user) match {
        case None =>
          // First time in - welcome and enter
          val (finalState, enterMessages) = addToRoom(user, toRoom)

          (finalState, Seq(WelcomeUser(user)) ++ enterMessages)

        case Some(currentRoom) if currentRoom == toRoom =>
          (this, Seq(SendToUser(user, "#info You are already in that room!")))

        case Some(_) =>
          // Already in - move from one room to another
          val (intermediateState, leaveMessages) = removeFromCurrentRoom(user)
          val (finalState, enterMessages)        = intermediateState.addToRoom(user, toRoom)

          (finalState, leaveMessages ++ enterMessages)
      }

    case Disconnect(user) => {
      val nextTable = table.find { case (seat, player) => player.userId == user } match {
        case None        => table
        case Some(value) => table - value._1
      }

      val nextPlayers = players - user
      println(nextPlayers)
      (this.copy(table = nextTable, players = nextPlayers), Seq(SendToAll(s"$user has left table")))
    }

    case Check(user) => {
      println("checking")
      // println(deck)
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
    (nextState, nextState.sendToRoom(room, s"#info Observer has joined the room"))
  }
}
