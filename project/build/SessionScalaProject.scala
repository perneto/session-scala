import sbt._

class SessionScalaProject(info: ProjectInfo) extends ParentProject(info)
{
  override def parallelExecution = true

  val scalaToolsSnapshots = ScalaToolsSnapshots
  val jbossNexus = "JBoss Nexus Repository" at "https://repository.jboss.org/nexus/content/groups/public/"

  class Runtime(info: ProjectInfo) extends DefaultProject(info) {
    override def fork = forkRun
    val rabbitmq = "com.rabbitmq" % "amqp-client" % "1.7.2"
    val scalatest = "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.RC6-SNAPSHOT"
  }

  class Examples(info: ProjectInfo) extends DefaultProject(info) {
    override def fork = forkRun
  }

  class CompilerPlugin(info: ProjectInfo) extends DefaultProject(info) {
    val scalatest = "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.RC6-SNAPSHOT"

  //val scribble_common = "org.scribble" % "scribble-common" % "1.0.0-SNAPSHOT"
  //val scribble_protocol = "org.scribble" % "scribble-protocol" % "1.0.0-SNAPSHOT"
  //val scribble_parser = "org.scribble" % "scribble-protocol-parser" % "1.0.0-SNAPSHOT"
  //val scribble_projection = "org.scribble" % "scribble-protocol-projection" % "1.0.0-SNAPSHOT"
  }

  lazy val runtime = project("runtime", "runtime", new Runtime(_))
  lazy val compilerplugin = project("compilerplugin", "compilerplugin", new CompilerPlugin(_), runtime)
  lazy val examples = project("examples", "examples", new Examples(_), runtime) //, compiler-plugin when ready
}
