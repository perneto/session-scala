package uk.ac.ic.doc.sessionscala.compiler

import java.util.{List => JList}
import tools.nsc.util.BatchSourceFile
import scalaj.collection.Imports._
import tools.nsc.Global
import org.scribble.protocol.model.{MessageSignature, ImportList, TypeReference}

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
      //println("scribbleToScala: " + tref + " -> " + ret)
      ret
    }

    def isSubtype(subtype: TypeReference, supertype: TypeReference, imports: JList[ImportList]) =
      isSubType(scribbleToScala(imports.asScala, subtype),
                scribbleToScala(imports.asScala, supertype))

  }

  //def scalaToScribble(label: Option[String], tpe: Option[Type]): MessageSignature =
  //  messageSignature(label, tpe map (typeSystem.scalaToScribble(_)))

  val typeSystem = ScalaTypeSystem

  def sig(tpe: Type) = MsgSig(None, Some(tpe))
  def sig(label: String, tpe: Type) = MsgSig(Some(label), Some(tpe))
  def sig(label: String) = MsgSig(Some(label), None)
  case class MsgSig(label: Option[String], msgType: Option[Type]) {
    def toScribble = {
      if (label.isEmpty && msgType.isEmpty)
        throw new IllegalArgumentException("At least one of label and message type should be specified")
      val msig =  new MessageSignature()
      label foreach (l => msig.setOperation(l))
      msgType foreach  (t => msig.getTypeReferences.add(typeSystem.scalaToScribble(t)))
      msig
    }
  }

  case class ScalaTypeReference(t: Type) extends TypeReference {
    override def toString = "ScalaTypeRef[" + t + "]"

    override def getName = "scala[" + t + "]"
  }
}