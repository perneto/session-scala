import sbt._

class SessionScalaProject(info: ProjectInfo) extends ParentProject(info)
{
  override def parallelExecution = true

  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" %
      "1.0.1-for-scala-2.8.0.RC1-SNAPSHOT"

  lazy val runtime = project("runtime") // one argument: don't use default project structure
  lazy val compilerplugin = project("compilerplugin", "compilerplugin", runtime) // 2 arguments: default project structure
  lazy val examples = project("examples", runtime) //, compiler-plugin when ready
}
