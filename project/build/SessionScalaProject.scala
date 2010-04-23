import sbt._

class SessionScalaProject(info: ProjectInfo) extends DefaultProject(info)
{
  override def parallelExecution = true

  val scalaToolsRepo = "Scala-Tools Maven Repository" at
      "http://nexus.scala-tools.org/content/repositories/snapshots/"

  val scalatest = "org.scalatest" % "scalatest" %
      "1.0.1-for-scala-2.8.0.Beta1-with-test-interfaces-0.3-SNAPSHOT" % "test"

  val rabbitmq = "com.rabbitmq" % "amqp-client" % "1.7.2"

  lazy val runtime = project("runtime", "runtime")
  lazy val compilerplugin = project("compilerplugin", "compilerplugin", runtime)
  lazy val examples = project("examples", "examples", runtime) //, compiler-plugin when ready
}
