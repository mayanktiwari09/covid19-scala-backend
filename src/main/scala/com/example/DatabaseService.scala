package com.example

import org.postgresql.Driver
import com.example.reuse.{SystemUtils, EnvironmentUtils}
import com.zaxxer.hikari.HikariConfig
import org.json.JSONObject
import cats.effect.{Blocker, Resource}
import com.zaxxer.hikari.HikariDataSource
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts
import monix.eval.Task
import org.springframework.jdbc.core.JdbcTemplate

case class PostgresProperties(hostname: String, port: String, dbName: String, username: String, password: String)

object DatabaseService {

  val postgresService = SystemUtils.getEnvPropertyFromServices[PostgresProperties]("user-provided", Option("covid19-DB"), postgresParamMapper)
  val ds = DatabaseService.createDataSource()
  val transactor = createTransactor(ds)

  val jdbcTemplate = new JdbcTemplate(ds)

  type JR = Resource[Task, HikariTransactor[Task]]

  private def createTransactor(ds: HikariDataSource): JR =
    for {
      ce <- ExecutionContexts.fixedThreadPool[Task](32)
      be <- Blocker[Task]
    } yield HikariTransactor[Task](ds, ce, be)

  def createDataSource(autoCommit: Boolean = false, connectionTimeout: Long = 60000,
                       idleTimeout: Long = 30000, maxLifetime: Long = 180000,
                       maxPoolSize: Int = 50, minIdlePoolSize: Int = 2): HikariDataSource = {

    val hc = new HikariConfig()

    hc.setPoolName("PostgreSQL Connection Pool")
    hc.setConnectionTestQuery("SELECT 1")
    hc.setDriverClassName(classOf[Driver].getName)

    hc.setConnectionTimeout(connectionTimeout)
    hc.setIdleTimeout(idleTimeout)
    hc.setMaxLifetime(maxLifetime)

    hc.setMaximumPoolSize(maxPoolSize)
    hc.setMinimumIdle(minIdlePoolSize)

    hc.setJdbcUrl(host)
    hc.setUsername(user)
    hc.setPassword(password)

    hc.setAutoCommit(autoCommit)

    new HikariDataSource(hc)
  }

  def host: String = {
    postgresService match {
      case Some(value) => s"""jdbc:postgresql://${value.hostname}:${value.port}/${value.dbName}"""
      case None => throw new RuntimeException("Could not read Postgres")
    }
  }

  def user: String = {
    postgresService match {
      case Some(value) => value.username
      case None => throw new RuntimeException("Could not read Hana")
    }
  }

  def password: String = {
    postgresService match {
      case Some(value) => value.password
      case None => throw new RuntimeException("Could not read Hana")
    }
  }

  def getPostgresURI: String = {

    EnvironmentUtils.environment match {
      case EnvironmentUtils.cf => {
        postgresService match {
          case Some(pgService) =>
            s"jdbc:postgresql://${pgService.username}:${pgService.password}@${pgService.hostname}:${pgService.port}/${pgService.dbName}"

          case _ => throw new RuntimeException("Could not read Hana")
        }
      }
    }
  }

  def postgresParamMapper[hanaService](envObject: JSONObject): PostgresProperties = {
    val postgresCredentials = envObject.get("credentials").asInstanceOf[JSONObject]
    PostgresProperties(postgresCredentials.get("hostname").toString(), postgresCredentials.get("port").toString(),
      postgresCredentials.get("dbname").toString(), postgresCredentials.get("username").toString(), postgresCredentials.get
      ("password").toString())
  }


}