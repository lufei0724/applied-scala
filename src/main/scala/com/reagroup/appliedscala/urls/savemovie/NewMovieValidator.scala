package com.reagroup.appliedscala.urls.savemovie

import cats.data.Validated._
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._

object NewMovieValidator {

  /**
    * How do we combine two validations together? Refer to `ValidationExercises` for a refresher!
    *
    * Hint: `Validated` has an Applicative instance.
    */
  def validate(newMovie: NewMovieRequest): ValidatedNel[MovieValidationError, ValidatedMovie] = {
    validateMovieName(newMovie.name).productR(validateMovieSynopsis(newMovie.synopsis)) match {
      case Valid(_) => Valid(ValidatedMovie(newMovie.name, newMovie.synopsis))
      case Invalid(e) => Invalid(e)
    }
  }

  /**
    * If `name` is empty, return an `InvalidNel` containing `MovieNameTooShort`,
    * else return a `Valid` containing the `name`.
    *
    * Hint: You can use `.isEmpty` or `.nonEmpty` on `String`
    */
  private def validateMovieName(name: String): ValidatedNel[MovieValidationError, String] =
    if (name.isEmpty) Validated.invalidNel(MovieNameTooShort)
    else Validated.validNel(name)

  /**
    * If `synopsis` is empty, return an `InvalidNel` containing `MovieSynopsisTooShort`,
    * else return a `Valid` containing the `synopsis`.
    *
    * Hint: You can use `.isEmpty` or `.nonEmpty` on `String`
    */
  private def validateMovieSynopsis(synopsis: String): ValidatedNel[MovieValidationError, String] =
    if (synopsis.isEmpty) Validated.invalidNel(MovieSynopsisTooShort)
    else Validated.validNel(synopsis)

}
