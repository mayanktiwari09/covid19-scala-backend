package com.example.reuse

import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper, SerializationFeature}
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.ScalaObjectMapper

object JsonDataFormatter {

  val objectMapper = new ObjectMapper() with ScalaObjectMapper
  objectMapper.findAndRegisterModules()
  objectMapper.registerModule(DefaultScalaModule)
  objectMapper.registerModule(new JavaTimeModule)
  objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
  objectMapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS)

  /**
   * Method to deserialize JSON content from given JSON content String to a specified class object.
   *
   * @param str
   * @tparam T
   * @return
   */
  def deserialize[T: Manifest](str: String): T = {
    objectMapper.readValue[T](str)
  }

  /**
   * Serializes an object into a string value
   *
   * @param obj
   * @return
   */
  def serialize(obj: Any): String = {
    objectMapper.writeValueAsString(obj)
  }
}
