import sbt._

class SessionScalaProject(info: ProjectInfo) extends ParentProject(info) with IdeaProject
{
  override def parallelExecution = true

  val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"

  class Runtime(info: ProjectInfo) extends DefaultProject(info) with IdeaProject {
    override def fork = forkRun
    val rabbitmq = "com.rabbitmq" % "amqp-client" % "1.7.2"
    val scalatest = "org.scalatest" % "scalatest" % "1.2"
  }

  class Examples(info: ProjectInfo) extends DefaultProject(info) with IdeaProject {
    override def fork = forkRun
  }

  class CompilerPlugin(info: ProjectInfo) extends DefaultProject(info) with IdeaProject {
    val scalatest = "org.scalatest" % "scalatest" % "1.2"
    override def testAction = super.testAction dependsOn(`package`)

    val scribble_common = "org.scribble.bundles" % "org.scribble.common" % "2.0.0-SNAPSHOT"
    val scribble_protocol = "org.scribble.bundles" % "org.scribble.protocol" % "2.0.0-SNAPSHOT"
    val scribble_parser = "org.scribble.bundles" % "org.scribble.protocol.parser" % "2.0.0-SNAPSHOT"
    val scribble_projection = "org.scribble.bundles" % "org.scribble.protocol.projection" % "2.0.0-SNAPSHOT"
    val scribble_conformance = "org.scribble.bundles" % "org.scribble.protocol.conformance" % "2.0.0-SNAPSHOT"
    val antlr = "org.antlr" % "antlr-runtime" % "3.2" // shouldn't be needed, something funny with scribble deps

    val scalaj_collection = "org.scalaj" %% "scalaj-collection" % "1.0"
  }

  lazy val runtime = project("runtime", "runtime", new Runtime(_))
  lazy val compilerplugin = project("compilerplugin", "compilerplugin", new CompilerPlugin(_), runtime)
  lazy val examples = project("examples", "examples", new Examples(_), runtime) //, compiler-plugin when ready
}
