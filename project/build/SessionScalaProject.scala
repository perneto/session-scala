import sbt._

class SessionScalaProject(info: ProjectInfo) extends ParentProject(info)
{
  override def parallelExecution = true

  val scalaToolsRepo = "Scala-Tools Maven Repository" at
      "http://nexus.scala-tools.org/content/repositories/snapshots/"

  val scalatest = "org.scalatest" % "scalatest" %
      "1.0.1-for-scala-2.8.0.Beta1-with-test-interfaces-0.3-SNAPSHOT" % "test"

  lazy val runtime = project("runtime") // one argument: don't use default project
  lazy val compilerplugin = project("compilerplugin", "compilerplugin", runtime) // 2 arguments: default project
  lazy val examples = project("examples", "examples", runtime) //, compiler-plugin when ready
}
