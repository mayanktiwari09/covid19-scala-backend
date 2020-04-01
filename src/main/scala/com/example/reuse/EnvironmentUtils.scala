package com.example.reuse

object EnvironmentUtils {

  val cf = "CF"
  val neo = "Neo"
  val local = "Local"

  val environment: String = System.getenv("ENVIRONMENT") match {
    case cf => cf
    case neo => neo
    case _ => local
  }
}
