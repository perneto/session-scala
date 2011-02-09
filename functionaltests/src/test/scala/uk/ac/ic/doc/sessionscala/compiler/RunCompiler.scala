package uk.ac.ic.doc.sessionscala.compiler

import scala.tools.nsc._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{Suite, FunSuite}
import java.lang.String
import java.io.{FileReader, BufferedReader, File}

/**
 * Created by: omp08
 */
class RunCompiler(file: String) {
  val scalaVersion = "2.8.1"
  def runCompiler(file: String): Global = {
    val settings = new Settings
    settings.classpath.tryToSet(List(
        "project/boot/scala-"+scalaVersion+"/lib/scala-compiler.jar" +
        ":project/boot/scala-"+scalaVersion+"/lib/scala-library.jar" +
        ":runtime/target/scala_"+scalaVersion+"/classes" +
        ":compilerplugin/target/scala_"+scalaVersion+"/classes" +
        ":functionaltests/target/scala_"+scalaVersion+"/classes"
    ))
    //settings.showPlugins only works if you're not compiling a file, same as -help
    settings.require.tryToSet(List("sessiontyping"))
    settings.plugin.tryToSet(List(
      "compilerplugin/target/scala_"+scalaVersion+"/compilerplugin_"+scalaVersion+"-0.1.jar"))
    settings.pluginOptions.tryToSet(List())
    //settings.print.tryToSet(List("all"))
    //settings.verbose.tryToSet(List())
    val tmpdir = System.getProperty("java.io.tmpdir")
    settings.d.tryToSet(List(tmpdir))
    val compiler = new Global(settings)

    new compiler.Run().compile(List(file))
    compiler
  }  
}

object util {
  def scalaFilesInDir(dir: String): List[String] = scalaFilesInDir(new File(dir))
  def scalaFilesInDir(dir: File): List[String] = {
    if (!dir.isDirectory) throw new IllegalArgumentException(dir + " does not exist or is not a directory")
    val arr: Array[String] = dir.list()
    List(arr: _*) flatMap { name =>
      val path = new File(dir,name)
      if (path.isDirectory)
        scalaFilesInDir(path)
      else if (path.getName.endsWith(".scala")) {
        if (disabled(path)) Nil
        else List(path.getAbsolutePath)
      }
      else Nil
    }
  }

  def testcase = System.getProperty("testcase")

  def disabled(path: File): Boolean = {
    (testcase != null && !path.getName.matches(".*" + testcase + ".*")) ||
      withBufferedReader(path) { reader =>
        reader.readLine.matches("//DISABLED")
      }
  }

  def withBufferedReader[T](path: File)(block: BufferedReader => T): T = {
    var reader: BufferedReader = null
    try {
      reader = new BufferedReader(new FileReader(path))
      block(reader)
    } finally {
      if (reader != null) reader.close()
    }
  }

  def findFiles(dir: String, constructor: String => Suite): List[Suite] = {
    val list = util.scalaFilesInDir(dir)
    if (list.isEmpty && testcase != null)
      println("WARNING: Could not find testcase: " + testcase)
    else if (testcase != null)
      println("Running testcase(s): " + (list reduceLeft (_ + ", " + _)) + " (selected with -Dtestcase="+testcase+")")
    list.map(constructor)
  }
}

class RunCompileOk extends Suite {
  class FileShouldCompile(file: String) extends RunCompiler(file) with FunSuite with ShouldMatchers {
    test("file should compile successfully: " + file) {
      val compiler = runCompiler(file)

      assert(!compiler.reporter.hasErrors, "The file " + file + " should have compiled successfully")
    }
  }

  override def nestedSuites = {
    util.findFiles("functionaltests/src/main/scala/compileok/", (new FileShouldCompile(_)))
  }
}

class RunCompileError extends Suite {
  class FileShouldHaveErrors(file: String) extends RunCompiler(file) with FunSuite with ShouldMatchers {
    test("file should compile with errors: " + file) {
      val compiler = runCompiler(file)

      assert(compiler.reporter.hasErrors, "The file " + file + " should have compiled with errors")
    }
  }

  override def nestedSuites = {
    util.findFiles("functionaltests/src/main/scala/compileerror/", (new FileShouldHaveErrors(_)))
  }
}