import sbt._


class SessionScalaProject(info: ProjectInfo) extends ParentProject(info)
{
  override def parallelExecution = true

  val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"
  val mavenRepo = DefaultMavenRepository
  val scalatools = ScalaToolsReleases

  class Runtime(info: ProjectInfo) extends DefaultProject(info) {
    override def fork = forkRun
    val rabbitmq = "com.rabbitmq" % "amqp-client" % "2.3.1"

    val scribble_common = "org.scribble.bundles" % "org.scribble.common" % "2.0.0-SNAPSHOT"
    val scribble_protocol = "org.scribble.bundles" % "org.scribble.protocol" % "2.0.0-SNAPSHOT"
    val scribble_parser = "org.scribble.bundles" % "org.scribble.protocol.parser" % "2.0.0-SNAPSHOT"

    val scalatest = "org.scalatest" % "scalatest" % "1.2"
    val scalaj_collection = "org.scalaj" % "scalaj-collection_2.8.0" % "1.0"
  }

  class ForkingProject(info: ProjectInfo) extends DefaultProject(info) {
    override def fork = forkRun
  }

  class FunctionalTests(info: ProjectInfo) extends ForkingProject(info) {
    def dep(t: Task) = t.dependsOn(compilerplugin.`proguard`)
    override def testAction = extraDep(super.testAction)
    override def testOnlyAction = task { args => extraDep(super.testOnlyAction(args)) }
  }

  class CompilerPlugin(info: ProjectInfo) extends DefaultProject(info) with ProguardProject {
    val scribble_conformance = "org.scribble.bundles" % "org.scribble.protocol.conformance" % "2.0.0-SNAPSHOT"
    val scribble_projection = "org.scribble.bundles" % "org.scribble.protocol.projection" % "2.0.0-SNAPSHOT"

    val antlr = "org.antlr" % "antlr-runtime" % "3.2" // shouldn't be needed, something funny with scribble deps

    override def proguardOptions = List(
      "-keep class uk.ac.ic.doc.sessionscala.** { *; }",
      "-keep class org.scribble.** { *; }",
      "-keep class org.osgi.framework.* { *; }",
      "-keep class org.osgi.util.tracker.** { *; }",
      "-keep class org.antlr.** { *; }",
      "-keep class antlr.** { *; }",
      "-keep class com.rabbitmq.** { *; }"
    )
    override def proguardLibraryJars = super.proguardLibraryJars +++ scalaLibraryPath
    lazy val felixCompilerPluginJar = managedDependencyPath  / "compile" / "org.apache.felix.framework-3.0.1.jar"
    lazy val felixRuntimeJar = runtime.managedDependencyPath / "compile" / "org.apache.felix.framework-3.0.1.jar"
    lazy val scalatestRuntimeJar = runtime.managedDependencyPath / "compile" / "scalatest-1.2.jar"

    override def proguardExclude = super.proguardExclude +++ felixCompilerPluginJar +++
            felixRuntimeJar +++ scalatestRuntimeJar
    override def makeInJarFilter(file :String) = file match {
      case _ => super.makeInJarFilter(file) + ",!META-INF/**" + ",!OSGI-INF/**" + ",!**/*.html" + "!images/**"
    }
  }

  lazy val runtime = project("runtime", "runtime", new Runtime(_))
  lazy val compilerplugin = project("compilerplugin", "compilerplugin", new CompilerPlugin(_), runtime)
  lazy val examples = project("examples", "examples", new ForkingProject(_), runtime)
  lazy val functionaltests = project("functionaltests", "functionaltests", new FunctionalTests(_), runtime, compilerplugin)

}
