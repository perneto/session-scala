package uk.ac.ic.doc.sessionscala.compiler

import scala.tools.nsc._
import org.scalatest.{Suite, FunSuite}
import java.lang.String

class RunCompiler(file: String, classpath: String) {
  def runCompiler(file: String): Global = {
    val settings = new Settings
    settings.classpath.tryToSet(List(
      "/usr/local/Cellar/scala/"+scalaVersion+"/libexec/lib/scala-compiler.jar"+
      ":/usr/local/Cellar/scala/"+scalaVersion+"/libexec/lib/scala-library.jar"+      
      classpath
    ))
    //settings.showPlugins only works if you're not compiling a file, same as -help
    settings.require.tryToSet(List("sessiontyping"))
    settings.plugin.tryToSet(List(
      "compilerplugin/target/scala_"+scalaVersion+"/compilerplugin_"+scalaVersion+"-0.1.min.jar"))
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

trait RunCompileOk extends Suite {
  val classpath: String
  class FileShouldCompile(file: String) extends RunCompiler(file, classpath) with FunSuite {
    test("file should compile successfully: " + file) {
      val compiler = runCompiler(file)

      assert(!compiler.reporter.hasErrors, "The file " + file + " should have compiled successfully")
    }
  }

  override def nestedSuites = {
    findFiles("functionaltests/src/main/scala/compileok/", (new FileShouldCompile(_)))
  }
}

trait Packaged {
  val classpath = 
    ":runtime/target/scala_"+scalaVersion+"/runtime_"+scalaVersion+"-0.1.min.jar"+
    ":functionaltests/target/scala_"+scalaVersion+"/classes" // for tests that use externally defined classes
  
}
trait CompiledClasses {
  val classpath = 
    ":runtime/target/scala_"+scalaVersion+"/classes"+
    ":functionaltests/target/scala_"+scalaVersion+"/classes"
}

trait RunCompileError extends Suite {
  val classpath: String
  class FileShouldHaveErrors(file: String) extends RunCompiler(file, classpath) 
  with FunSuite {
    test("file should compile with errors: " + file) {
      val compiler = runCompiler(file)

      assert(compiler.reporter.hasErrors, "The file " + file + " should have compiled with errors")
    }
  }

  override def nestedSuites = {
    findFiles("functionaltests/src/main/scala/compileerror/", (new FileShouldHaveErrors(_)))
  }
}

class RunCompileOkPackaged extends RunCompileOk with Packaged
class RunCompileOkClasses extends RunCompileOk with CompiledClasses
class RunCompileErrorPackaged extends RunCompileError with Packaged
class RunCompileErrorClasses extends RunCompileError with CompiledClasses