package com.jenovs.chatserver

import java.io.File
import java.util.concurrent.Executors

import cats.effect.concurrent.Ref
import cats.effect.{Blocker, ContextShift, Sync}
import fs2.concurrent.{Queue, Topic}
import fs2.{Pipe, Stream}
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.{Close, Text}
import org.http4s.{HttpRoutes, MediaType, StaticFile, Header}
import org.http4s.util.CaseInsensitiveString
import java.util.UUID
import org.http4s.Headers
import org.http4s.server.middleware.CORS

/*
 * Processes single HTTP requests
 */
class ChatRoutes[F[_]: Sync: ContextShift](
    chatState: Ref[F, ChatState],
    queue: Queue[F, InputMessage],
    topic: Topic[F, OutputMessage]
) extends Http4sDsl[F] {

  private val blocker = {
    val numBlockingThreadsForFilesystem = 4
    val blockingPool                    = Executors.newFixedThreadPool(numBlockingThreadsForFilesystem)
    Blocker.liftExecutorService(blockingPool)
  }

  val routes: HttpRoutes[F] =
    HttpRoutes.of[F] {
      // Static resources
      case request @ GET -> Root =>
        def getUUID() = UUID.randomUUID().toString()
        val token = request.headers.get(CaseInsensitiveString("Token")) match {
          case None => getUUID()
          case Some(header) =>
            if (header.value.isEmpty) getUUID() else header.value.split(",").takeRight(1).mkString
        }
        Ok(Header("Token", token))

      // Bind a WebSocket connection for a user
      case request @ GET -> Root / "ws" / token =>
        val userName = token
        // Routes messages from our "topic" to a WebSocket
        val toClient: Stream[F, WebSocketFrame.Text] =
          topic
            .subscribe(1000)
            .filter(_.forUser(userName))
            .map(msg => Text(msg.toString))

        // Function that converts a stream of one type to another. Effectively an external "map" function
        def processInput(wsfStream: Stream[F, WebSocketFrame]): Stream[F, Unit] = {
          // Stream of initialization events for a user
          val entryStream: Stream[F, InputMessage] =
            Stream.emits(Seq(EnterRoom(userName, InputMessage.DefaultRoomName)))

          // Stream that transforms between raw text from the client and parsed InputMessage objects
          val parsedWebSocketInput: Stream[F, InputMessage] =
            wsfStream
              .collect {
                case Text(text, _) => InputMessage.parse(userName, text)

                // Convert the terminal WebSocket event to a User disconnect message
                case Close(_) => Disconnect(userName)
              }

          // Create a stream that has all of the user input sandwiched between the entry and disconnect messages
          (entryStream ++ parsedWebSocketInput).through(queue.enqueue)
        }

        // WebSocketBuilder needs a "pipe" which is a type alias for a stream transformation function like processInput above
        // This variable is not necessary to compile, but is included to clarify the exact type of Pipe.
        val inputPipe: Pipe[F, WebSocketFrame, Unit] = processInput

        // Build the WebSocket handler
        val headers = Headers.of(
          Header("Access-Control-Expose-Headers", "*"),
          Header("Token", token)
        )
        WebSocketBuilder[F].build(toClient, inputPipe)
    }
}
