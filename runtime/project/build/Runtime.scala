import sbt._

class Runtime(info: ProjectInfo) extends DefaultProject(info) {
  override def fork = forkRun

  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" %
      "1.0.1-for-scala-2.8.0.RC1-SNAPSHOT"

  val rabbitmq = "com.rabbitmq" % "amqp-client" % "1.7.2"
}

