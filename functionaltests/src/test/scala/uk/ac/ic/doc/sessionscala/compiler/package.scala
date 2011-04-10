package uk.ac.ic.doc.sessionscala

import scala.tools.nsc._
import org.scalatest.Suite
import java.lang.String
import java.io.{FileReader, BufferedReader, File}

package object compiler {
  val scalaVersion = "2.8.1"
    
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
    val list = scalaFilesInDir(dir)
    if (list.isEmpty && testcase != null)
      println("WARNING: Could not find testcase: " + testcase)
    else if (testcase != null)
      println("Running testcase(s): " + (list reduceLeft (_ + ", " + _)) + " (selected with -Dtestcase="+testcase+")")
    list.map(constructor)
  }
}













