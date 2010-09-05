package uk.ac.ic.doc.sessionscala.compiler

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scribble.protocol.model.TypeReference

class TypeMappingTest extends FunSuite
                      with SessionTypingEnvironments
                      with ScalaCompilerSetup
                      with ScribbleParsing
                      with ShouldMatchers {

  import global._

  test("map object type, scala -> scribble") {
    ScalaTypeSystem.scalaToScribble(
      definitions.getClass("scala.None").tpe) should be === (new TypeReference("None"))
  }

  test("map object type, scribble -> scala") {
    ScalaTypeSystem.scribbleToScala(Nil, new TypeReference("None")) should be === (
            definitions.getClass("scala.None").tpe
            )
  }

  test("map user-defined object") {
    ScalaTypeSystem.scalaToScribble(
    definitions.getModule("compileok.buyerseller.OK").tpe) should be === (new TypeReference("OK"))
      
  }
}