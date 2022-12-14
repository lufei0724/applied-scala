package com.reagroup.appliedscala.models

import io.circe.{Encoder, Json}
import io.circe.generic.semiauto._
import io.circe.syntax.EncoderOps

case class Movie(name: String, synopsis: String, reviews: Vector[Review])

object Movie {

  /**
    * Add an Encoder instance here
    *
    * Hint: Use `deriveEncoder`
    */

  implicit val encoder: Encoder[Movie] =
    Encoder { movie =>
      Json.obj(
        ("name", movie.name.asJson),
        ("synopsis", movie.synopsis.asJson),
        ("reviews", movie.reviews.asJson)
      )
    }
}