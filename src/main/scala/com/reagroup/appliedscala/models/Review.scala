package com.reagroup.appliedscala.models

import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.semiauto._
import io.circe.syntax.EncoderOps

case class Review(author: String, comment: String)

object Review {

  /**
    * Add an Encoder instance here
    *
    * Hint: Use `deriveEncoder`
    */

  implicit val encoder: Encoder[Review] =
    Encoder { review =>
      Json.obj(
        ("author", review.author.asJson),
        ("comment", review.comment.asJson)
      )
    }
}
