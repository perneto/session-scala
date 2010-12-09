package uk.ac.ic.doc.sessionscala.compiler

import java.util.{List => JList}
import org.scribble.protocol.model.{ImportList, TypeReference}
import tools.nsc.util.BatchSourceFile
import scalaj.collection.Imports._
import tools.nsc.Global

/**
 * Created by: omp08
 */

trait ScalaTypeSystemComponent {
  val global: Global
  import global._

  object ScalaTypeSystem extends HostTypeSystem {
    lazy val rootCtx = global.analyzer.rootContext(
        new CompilationUnit(new BatchSourceFile("<sessiontyping>", "")))

    def scalaToScribble(t: Type): TypeReference = ScalaTypeReference(t)

    def scribbleToScala(imports: Seq[ImportList], tref: TypeReference): Type = {
      val ret = if (tref.isInstanceOf[ScalaTypeReference]) {
        tref.asInstanceOf[ScalaTypeReference].t
        // with scala 2.8.1, using pattern matching here fails (at least with t = Int)
        // (skips the ScalaTypeReference branch and goes to case _)
      } else {
        val found: Seq[Type] = imports.map({i: ImportList =>
          val javaPackage = i.getLocation
          assert(javaPackage != null)

          val typeImport = i.getTypeImport(tref.getName)
          if (typeImport != null) {
            val dataType = typeImport.getDataType
            assert(dataType.getFormat == "java")
            val javaClassName = dataType.getDetails
            val fullname = javaPackage + "." + javaClassName
            Some(definitions.getClass(fullname).tpe)
          } else None
        }).flatten
        if (found.length == 1) found(0)
        else if (found.length > 2) throw new IllegalStateException(
          "Should not happen: found more than 1 matching import for " + tref)
        else {
          val l = rootCtx.imports.map(_.importedSymbol(newTypeName(tref.getName)))
                  .filter(_ != NoSymbol)
          if (l.length >= 1) l(0).tpe // order: scala.Predef, scala, java.lang
          else throw new SessionTypeCheckingException("Could not find pre-defined type: " + tref.getName + ". Imports list is: " + imports)
        }
      }
      println("scribbleToScala: " + tref + " -> " + ret)
      ret
    }

    def isSubtype(subtype: TypeReference, supertype: TypeReference, imports: JList[ImportList]) =
      isSubType(scribbleToScala(imports.asScala, subtype),
                scribbleToScala(imports.asScala, supertype))

  }

  val typeSystem = ScalaTypeSystem

  case class ScalaTypeReference(t: Type) extends TypeReference {
    override def toString = "ScalaTypeRef[" + t + "]"

    override def getName = "scala[" + t + "]"
  }
}