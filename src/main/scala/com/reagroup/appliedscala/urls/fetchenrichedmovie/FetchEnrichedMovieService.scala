package com.reagroup.appliedscala.urls.fetchenrichedmovie

import cats.effect.IO
import com.reagroup.appliedscala.models._
import cats.implicits._

class FetchEnrichedMovieService(fetchMovie: MovieId => IO[Option[Movie]],
                                fetchMetascore: MovieName => IO[Option[Metascore]]) {

  /**
    * In order to construct an `EnrichedMovie`, we need to first get a `Movie` and a `Metascore`.
    * We can do so using the functions that are passed in as dependencies.
    *
    * For the purpose of this exercise, let's raise an `EnrichmentFailure` if the `Metascore` does not exist.
    *
    * Hint: We know we are going to be chaining multiple effects in `IO` so let's start a for-comprehension.
    * Also pattern match on `Option` if you're stuck!
    * */
  def fetch(movieId: MovieId): IO[Option[EnrichedMovie]] = for {
    optionMovie <- fetchMovie(movieId)
    enrichedMovie <- optionMovie.map(enrichMovieWithMetascore) match {
      case Some(c) => c.map(Some(_))
      case _ => IO.pure(None)
    }
    //enrichedMovie <- enrichMovieWithMetascore(movie)
  } yield enrichedMovie

  /**
    * Given a `Movie`, we can call `fetchMetascore` using the `name` of the `Movie`.
    * If no `Metascore` is found, raise an `EnrichmentFailure` using `IO.raiseError`.
    * */
  private def enrichMovieWithMetascore(movie: Movie): IO[EnrichedMovie] = for {
    optionOrMetascore  <- fetchMetascore(movie.name)
    enrichedMovie <- optionOrMetascore match {
      case Some(m) => IO.pure(EnrichedMovie(movie, m))
      case _ => IO.raiseError[EnrichedMovie](EnrichmentFailure(movie.name))
    }
  } yield enrichedMovie
}