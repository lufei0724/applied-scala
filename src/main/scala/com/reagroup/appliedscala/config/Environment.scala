package com.reagroup.appliedscala.config

import cats.data.ValidatedNel
import cats.implicits._

final case class Environment(env: Map[String, String]) {
  final def optional(key: String, defaultValue: => String): ValidatedNel[ConfigError, String] = {
    env.getOrElse(key, defaultValue).validNel
  }

  final def required(key: String): ValidatedNel[ConfigError, String] = {
//    key match {
//      case "OMDB_API_KEY" => "7f9b5b06".validNel
//      case "DATABASE_HOST" => "localhost".validNel
//      case "DATABASE_NAME" => "moviedb".validNel
//      case "DATABASE_USERNAME" => "moviedb".validNel
//      case "DATABASE_PASSWORD" => "".validNel
//      case _ => MissingEnvironmentVariable(key).invalidNel
//    }
    env.get(key) match {
      case Some(value) => value.validNel
      case None => MissingEnvironmentVariable(key).invalidNel
    }
  }
}
