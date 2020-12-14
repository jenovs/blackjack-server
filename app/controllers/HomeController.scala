package controllers

import java.net.URI
import javax.inject._

import akka.actor.ActorSystem
import akka.event.Logging
import akka.stream.Materializer
import akka.stream.scaladsl.{BroadcastHub, Flow, Keep, MergeHub, Source}
import play.api.Logger
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import java.util.UUID

import io.circe.syntax._

import Deck._

@Singleton
class HomeController @Inject() (
    val controllerComponents: ControllerComponents
)(implicit
    actorSystem: ActorSystem,
    mat: Materializer,
    executionContext: ExecutionContext,
    webJarsUtil: org.webjars.play.WebJarsUtil
) extends BaseController
    with RequestMarkerContext {

  private type WSMessage = String

  private val logger = Logger(getClass)

  private var deck = Deck().shuffle()

  private implicit val logging =
    Logging(actorSystem.eventStream, logger.underlyingLogger.getName)

  private val (playSink, playSource) = {
    val source = MergeHub
      .source[WSMessage]
      .map(n => {
        val (card, newDeck) = deck.takeOne().get
        deck = newDeck
        println("do stuff", n)
        card.asJson.noSpaces
      })
      .log("source")
      .recoverWithRetries(-1, { case _: Exception => Source.empty })

    val sink = BroadcastHub.sink[WSMessage]
    source.toMat(sink)(Keep.both).run()
  }

  private val userFlow: Flow[WSMessage, WSMessage, _] = {
    Flow.fromSinkAndSource(playSink, playSource)
  }

  def index: Action[AnyContent] = Action { implicit request: RequestHeader =>
    val token = request.headers.get("Token")

    token match {
      case None        => Ok.withHeaders("Token" -> UUID.randomUUID().toString())
      case Some(value) => Ok.withHeaders("Token" -> value)
    }
  }

  def play(): WebSocket = {
    WebSocket.acceptOrResult[WSMessage, WSMessage] {
      case rh if sameOriginCheck(rh) => {
        println("opening socket")
        Future
          .successful(userFlow)
          .map { flow =>
            println(rh.cookies)
            println(rh.headers.get("Sec-WebSocket-Protocol"))
            Right(flow)
          }
          .recover { case e: Exception =>
            val msg = "Cannot create websocket"
            logger.error(msg, e)
            val result = InternalServerError(msg)
            Left(result)
          }
      }

      case rejected =>
        logger.error(s"Request ${rejected} failed same origin check")
        Future.successful {
          Left(Forbidden("forbidden"))
        }
    }
  }

  /** Checks that the WebSocket comes from the same origin.  This is necessary to protect
    * against Cross-Site WebSocket Hijacking as WebSocket does not implement Same Origin Policy.
    *
    * See https://tools.ietf.org/html/rfc6455#section-1.3 and
    * http://blog.dewhurstsecurity.com/2013/08/30/security-testing-html5-websockets.html
    */
  private def sameOriginCheck(implicit rh: RequestHeader): Boolean = {
    // The Origin header is the domain the request originates from.
    // https://tools.ietf.org/html/rfc6454#section-7
    logger.debug("Checking the ORIGIN ")

    rh.headers.get("Origin") match {
      case Some(originValue) if originMatches(originValue) =>
        logger.debug(s"originCheck: originValue = $originValue")
        true

      case Some(badOrigin) =>
        logger.error(
          s"originCheck: rejecting request because Origin header value ${badOrigin} is not in the same origin"
        )
        false

      case None =>
        logger.error(
          "originCheck: rejecting request because no Origin header found"
        )
        false
    }
  }

  /** Returns true if the value of the Origin header contains an acceptable value.
    */
  private def originMatches(origin: String): Boolean = {
    true
    // try {
    //   val url = new URI(origin)
    //   url.getHost == "localhost" &&
    //     (url.getPort match { case 9000 | 19001 => true; case _ => false })
    // } catch {
    //   case e: Exception => false
    // }
  }

}
