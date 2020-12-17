package com.jenovs.chatserver

import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._
import fs2.Stream
import fs2.concurrent.{Queue, Topic}
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.syntax.kleisli._
import org.http4s.server.middleware.CORS

import scala.concurrent.duration._
import scala.util.Try
import org.http4s.websocket.WebSocketFrame.Ping
import com.jenovs.chatserver.ChatState.Dealing

/*
 * Application entry point
 */
object ChatServer extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    // Get a tcp port that might be specified on the command line or in an environment variable
    val httpPort = args.headOption
      .orElse(sys.env.get("PORT"))
      .flatMap(s => Try(s.toInt).toOption) // Ignore any integer parse errors
      .getOrElse(8080)

    for (// Synchronization objects must be created at a level where they can be shared with every object that needs them
         queue <- Queue.unbounded[IO, InputMessage];
         topic <- Topic[IO, OutputMessage](SendToUsers(Set.empty, ""));

         // There are a few ways to represent state in this model. We choose functional references so that the
         // state can be referenced in multiple locations. If the state is only needed in a single location
         // then there are simpler models (like Stream.scan and Stream.mapAccumulate)
         ref <- Ref.of[IO, ChatState](ChatState());

         // Create and then combine the top-level streams for our application
         exitCode <- {
           // Stream for HTTP requests
           val httpStream = ServerStream.stream[IO](httpPort, ref, queue, topic)

           // Stream to keep alive idle WebSockets
           val keepAlive = Stream.fixedRate[IO](10.seconds).flatMap(_ => Stream.emit(Ping()))

           val scheduler = Stream
             .fixedRate[IO](1.second)
             .evalMap(_ => {
               for {
                 state                  <- ref.get
                 (nextState, isUpdated) = state.check()
                 _                      <- if (isUpdated) ref.set(nextState) else IO.unit
                 table                  = nextState.getTableJson()
               } yield if (isUpdated) Stream.emit(SendToAll(table)) else Stream.empty
             })
             .flatMap(identity)
             .through(topic.publish)

           // Stream to process items from the queue and publish the results to the topic
           // 1. Dequeue
           // 2. apply message to state reference
           // 3. Convert resulting output messages to a stream
           // 4. Publish output messages to the publish/subscribe topic
           val processingStream =
             queue.dequeue
               .evalMap(msg => ref.modify(_.process(msg)))
               .flatMap(Stream.emits)
               .through(topic.publish)

           // fs2 Streams must be "pulled" to process messages. Drain will perpetually pull our top-level streams
           Stream(httpStream, keepAlive, scheduler, processingStream).parJoinUnbounded.compile.drain
             .as(ExitCode.Success)
         }) yield exitCode
  }
}

object ServerStream {
  // Builds a stream for HTTP events processed by our router
  def stream[F[_]: ConcurrentEffect: Timer: ContextShift](
      port: Int,
      chatState: Ref[F, ChatState],
      queue: Queue[F, InputMessage],
      topic: Topic[F, OutputMessage]
  ): fs2.Stream[F, ExitCode] =
    BlazeServerBuilder[F]
      .bindHttp(port, "0.0.0.0")
      .withHttpApp(
        CORS(
          Router(
            "/" -> new ChatRoutes[F](chatState, queue, topic).routes
          ).orNotFound
        )
      )
      .serve
}
