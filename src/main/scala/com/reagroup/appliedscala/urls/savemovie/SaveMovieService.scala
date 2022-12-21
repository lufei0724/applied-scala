package com.reagroup.appliedscala.urls.savemovie

import cats.data.Validated.{Invalid, Valid}
import cats.data._
import cats.implicits._
import cats.effect.IO
import com.reagroup.appliedscala.models._

class SaveMovieService(saveMovie: ValidatedMovie => IO[MovieId]) {

  /**
    * Before saving a `NewMovieRequest`, we want to validate the request in order to get a `ValidatedMovie`.
    * Complete `NewMovieValidator`, then use it here before calling `saveMovie`.
    */
  def save(newMovieReq: NewMovieRequest): IO[ValidatedNel[MovieValidationError, MovieId]] = {
    val errorsOrValidatedMovie = NewMovieValidator.validate(newMovieReq)
    errorsOrValidatedMovie match {
      case Valid(validatedMovie) => saveMovie(validatedMovie).map(_.valid)
      case Invalid(e) => IO.pure(Invalid(e))
    }
  }
}
