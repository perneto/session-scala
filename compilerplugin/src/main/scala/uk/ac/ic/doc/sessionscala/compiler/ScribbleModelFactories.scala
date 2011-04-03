package uk.ac.ic.doc.sessionscala.compiler

import org.scribble.protocol.model._
import scalaj.collection.Imports._

/**
 * Created by: omp08
 */

trait ScribbleModelFactories {
  type LA = List[Activity]

  def createInteraction(src: Role, dst: Role, msgSig: MessageSignature) =
      new Interaction(src, dst, msgSig)

  //def createWhen(label: TypeReference): When =
  //  createWhen(label, Nil)
  def createWhen(label: TypeReference, block: LA): When =
    createWhen(new MessageSignature(label), block)
  def createWhen(label: MessageSignature, block: Block): When = {
    val w = new When
    w.setMessageSignature(label)
    w.setBlock(block)
    w
  }
  def createWhen(label: MessageSignature, block: LA): When = {
    val b = new Block
    b.getContents.addAll(block asJava)
    createWhen(label, b)
  }

  def emptyBody(branches: List[TypeReference]): List[(TypeReference, LA)] =
    branches.zipAll(Nil, null, Nil)

  def createChoice(src: Role, dst: Role, branches: List[(TypeReference, LA)]): Choice = {
    val c = new Choice
    c.setFromRole(src)
    c.setToRole(dst)
    c.getWhens().addAll(
      (for ((tref, block) <- branches) yield createWhen(tref,block)) asJava
    )
    c
  }
  def createChoice(src: Role, msig: MessageSignature, block: LA): Choice = {
    val c = new Choice
    c.setFromRole(src)
    c.getWhens.add(createWhen(msig, block))
    c
  }
  def createChoice(dst: Role, branches: List[(MessageSignature, LA)]): Choice = {
    val c = new Choice
    c.setToRole(dst)
    for ((label, block) <- branches) c.getWhens.add(createWhen(label, block))
    c
  }
  def createChoice(orig: Choice, branches: Seq[When]): Choice = {
    val c = new Choice
    c.setToRole(orig.getToRole)
    c.setFromRole(orig.getFromRole)
    branches.foreach(c.getWhens.add(_))
    c
  }
  def addToChoice(c: Choice, w: When) = {
    val newC = new Choice
    newC.setToRole(c.getToRole)
    newC.setFromRole(c.getFromRole)
    c.getWhens foreach (newC.getWhens.add(_))
    newC.getWhens.add(w)
    newC
  }

  def createRecBlock(label: String, block: LA): RecBlock =
    createRecBlock(label, createBlock(block))
  def createRecBlock(label: String, block: Block): RecBlock = {
    val r = new RecBlock
    r.setLabel(label)
    r.setBlock(block)
    r
  }

  def createBlock(contents: Seq[Activity]) = {
    val b = new Block
    contents foreach (b.add(_))
    b
  }

  def createRecursion(label: String): Recursion = {
    val r = new Recursion
    r.setLabel(label)
    r
  }

  def notEmpty(recur: RecBlock) = !recur.getBlock.getContents.isEmpty

  def isChoiceReceive(c: Choice) = !Session.isSendChoice(c)

}