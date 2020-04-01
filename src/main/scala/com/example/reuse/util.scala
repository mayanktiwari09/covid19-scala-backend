package com.example.reuse

import java.time.temporal.{ChronoUnit, TemporalUnit}
import cats.implicits._
import org.json.{JSONArray, JSONObject, JSONTokener}

import scala.collection.JavaConverters._

final case class Destination(name: String, url: String)

object SystemUtils {

  val VCAP_SERVICES = "VCAP_SERVICES"
  val VCAP_APPLICATION = "VCAP_APPLICATION"
  val AC_HOST = "ac-host"
  val HOST = "host"
  val CC_HOST = "url"
  val CC_CLIENT_ID = "clientid"
  val CC_CLIENT_SECRET = "clientsecret"
  val USER_PROVIDED = "user-provided"
  val XSUAA = "xsuaa"
  val AC_UAA = "ac-uaa"
  val AC_BROKER_UAA = "ac_broker-uaa"
  val CREDENTIALS = "credentials"
  val NAME = "name"
  val ENVIRONMENT = "environment"
  val DESTINATIONS = "destinations"

  /**
   *
   * @param key2 - The first level Key inside VCAP_SERVICES JSON object. eg: "user-provided", "xsuaa"
   * @param name - Value of the key called "name" of the parameter above. eg: "ac-host", "ac-uaa"
   * @param f    - Callback which returns the JSON object
   * @tparam T
   * @return - The callback with the type mentioned in the third parameter
   */
  def getEnvPropertyFromServices[T](key2: String, name: Option[String], f: JSONObject => T): Option[T] =
    getEnvProperty(Option(System.getenv(VCAP_SERVICES)), key2, name, f)

  /**
   *
   * @param key2
   * @param name
   * @param f
   * @tparam T
   * @return
   */
  def getEnvPropertyFromApplications[T](key2: String, name: Option[String], f: JSONObject => T): Option[T] =
    getEnvProperty(Option(System.getenv(VCAP_APPLICATION)), key2, name, f)

  //FIXME add fallback for On-Premise
  def getEnvProperty[T](key1: Option[String], key2: String, name: Option[String], callback: JSONObject => T)
  : Option[T] =
    key1.flatMap(value => {
      val jsonObject = new JSONObject(new JSONTokener(value))
      val arrayOption = Option(jsonObject.get(key2))
      arrayOption match {
        case Some(arrayValue) =>
          val jsonArray = new JSONArray(arrayValue.toString)
          name.map(curName => {
            val key3 = (0 until jsonArray.length).map(jsonArray.getJSONObject).map(v => {
              // FIXME: handle exception if .get is applied on None
              (v.get(NAME), v)
            }).toMap
            callback(key3(curName))
          })
        case None => throw new RuntimeException(s"""Not able to find the $name in system properties.""")
      }
    })

  def getAcHost: String = {
    val VCAP_SERVICES = getVcapServices
    val keys = VCAP_SERVICES.keys().asScala.toList
    val acKeyO = keys.find(key => key.asInstanceOf[String].contains("asset-central"))
    acKeyO match {
      case Some(value) => VCAP_SERVICES.getJSONArray(value.asInstanceOf[String]).getJSONObject(0).query("/credentials/endpoints/asset-central-service").asInstanceOf[String].stripSuffix("/")
      case None => endpoint(AC_HOST)
    }
  }

  def getDestinations: JSONArray = {
    val sDestinations = Option(System.getenv(DESTINATIONS))
    sDestinations match {
      case Some(value) => new JSONArray(value)
      case None => throw new RuntimeException(
        s"""Not able to find destinations. Maintain environment variable 'destinations' in the structure:
           |[{"name": "app1", "url": "https://url"}, {"name":"app2", "url": "https://url2"}]""".stripMargin)
    }
  }

  def endpoint(destinations: JSONArray, name: String) = {
    val destinationsL = JsonDataFormatter.deserialize[List[Destination]](destinations.toString())
    destinationsL.find(destination => destination.name == name) match {
      case Some(value) => value.url
      case None => ""
    }
  }

  def endpoint(name: String): String = {
    endpoint(getDestinations, name)
  }

  def getUPS(subKey: String): Option[JSONObject] =
    getEnvPropertyFromServices(USER_PROVIDED, subKey.some, JSONObject => JSONObject)

  // FIXME: handle exception if .get is applied on None
  def getAcUaaCredential(key: String): String =
    getEnvPropertyFromServices[String](XSUAA, AC_BROKER_UAA.some, (j: JSONObject) => j.getJSONObject(CREDENTIALS).getString(key)).get

  def getVcapServices: JSONObject = {
    getVcapVariables(VCAP_SERVICES)
  }

  def getVcapApplications: JSONObject = {
    getVcapVariables(VCAP_APPLICATION)
  }

  private def getVcapVariables(name: String): JSONObject = {
    val vcap = Option(System.getenv(name))
    vcap match {
      case Some(value) => new JSONObject(value)
      case _ => throw new RuntimeException(s"""Not able to find ${VCAP_SERVICES}.""")
    }
  }

}

object CacheConfigUtils {

  val defaultUnit = ChronoUnit.MINUTES
  val defaultDuration = 300
  val defaultSize = 100

  /**
   * To get the time unit out of a nested env variable i.e.
   * TEMPLATE_VARS : { TEMPLATE_CACHE_DURATION : 'value' }
   *
   * @param key
   * @return
   */
  def getTimeUnit(key: String): TemporalUnit =
    getTimeUnit(Option(System.getenv(key)))

  /**
   * This method will be used to get the time unit to set the cache. If the key is not found or the value in not in
   * NANOSECONDS, MICROSECONDS, MILLISECONDS, SECONDS, MINUTES, HOURS, DAYS then it is defaulted to MINUTES.
   *
   * @param timeUnit
   * @return
   */
  def getTimeUnit(timeUnit: Option[String]): TemporalUnit =
    timeUnit match {
      case Some(value) => value match {
        case "NANOSECONDS" => ChronoUnit.NANOS
        case "MICROSECONDS" => ChronoUnit.MICROS
        case "MILLISECONDS" => ChronoUnit.MILLIS
        case "SECONDS" => ChronoUnit.SECONDS
        case "MINUTES" => ChronoUnit.MINUTES
        case "HOURS" => ChronoUnit.HOURS
        case "DAYS" => ChronoUnit.DAYS
        case _ => defaultUnit
      }
      case None => defaultUnit
    }

  /**
   * To get the time unit out of an env variable i.e.
   * TEMPLATE_CACHE_DURATION : 'value'
   *
   * @param key
   * @return
   */
  def getTimeDuration(key: String): Long =
    getTimeDuration(Option(System.getenv(key)))

  /**
   * Returns the duration from a option or default as long.
   *
   * @param duration
   * @return
   */
  def getTimeDuration(duration: Option[String]): Long =
    duration match {
      case Some(value) => try {
        java.lang.Long.parseLong(value)
      } catch {
        case _: NumberFormatException => defaultDuration
      }
      case None => defaultDuration
    }

}
