package com.jenovs.server

import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._

object HandStatus {
  sealed abstract class HandStatus

  object HandStatus {
    case object Active    extends HandStatus
    case object Active0   extends HandStatus
    case object Active1   extends HandStatus
    case object Blackjack extends HandStatus
    case object Bust      extends HandStatus
    case object Standing  extends HandStatus
    case object Won       extends HandStatus
    case object Lost      extends HandStatus
    case object Push      extends HandStatus

    implicit val HandStatusEncoder: Encoder[HandStatus] = Encoder[String].contramap[HandStatus](_.toString)
  }

}
