package uk.ac.ic.doc.sessionscala.compiler

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scribble.protocol.model.TypeReference

class TypeMappingTest extends FunSuite
                      with ScalaTypeSystemComponent
                      with ScalaCompilerSetup
                      with ScribbleParsing
                      with ShouldMatchers {

  import global._

  lazy val t = definitions.getClass("scala.None").tpe

  test("map object type, scala -> scribble") {
    ScalaTypeSystem.scalaToScribble(t) should be === (ScalaTypeReference(t))
  }

  test("map object type, scribble -> scala") {
    ScalaTypeSystem.scribbleToScala(Nil, ScalaTypeReference(t)) should be === (t)
    ScalaTypeSystem.scribbleToScala(Nil, new TypeReference("None")) should be === (t)
  }

  test("map user-defined object") {
    val okTpe = definitions.getModule("buyerseller.OK").tpe
    ScalaTypeSystem.scalaToScribble(okTpe) should be === (ScalaTypeReference(okTpe))      
  }
}