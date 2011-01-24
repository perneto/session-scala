package uk.ac.ic.doc.sessionscala.compiler

import org.scribble.protocol.model._
import scalaj.collection.Imports._

/**
 * Created by: omp08
 */

trait TypeCheckingUtils {
  self: SessionTypedElementsComponent with ScribbleModelFactories with CommonEnvironments =>
  def checkSessionsRemainingSame(sessions1: Sessions, sessions2: Sessions): Unit = sessions1 foreach {
    case (chan, sessElse) =>
      val sessThen = sessions2(chan)
      if (sessElse.remaining != sessThen.remaining)
        throw new SessionTypeCheckingException(branchesUneven + " On channel: "
                  + chan + ", a branch had remaining session type: "
                  + sessThen.remaining + " while another had: " + sessElse.remaining)
  }

  def unroll(recur: LabelledBlock): Seq[Activity] = {
    def unrollRec(act: Activity): Activity = act match {
      case r: LabelledBlock if r.getLabel == recur.getLabel => r // masking
      case r: LabelledBlock => createLabelledBlock(r.getLabel, unrollRec(r.getBlock).asInstanceOf[Block])
      case rec: Recursion if rec.getLabel == recur.getLabel => recur
      case c: Choice => createChoice(c, (c.getWhens.asScala map (w => createWhen(w.getMessageSignature, unrollRec(w.getBlock).asInstanceOf[Block]))))
      case b: Block => createBlock(b.getContents.asScala.map(unrollRec(_)))
      case other => other
    }

    recur.getBlock.getContents.asScala.map(unrollRec(_))
  }

  def alphaRename(acts: Seq[Activity], oldLabel: String, newLabel: String): Seq[Activity] = {
    def alphaRenameRec(act: Activity): Activity = act match {
      case r: LabelledBlock if r.getLabel == oldLabel => r // masking
      case r: LabelledBlock => createLabelledBlock(r.getLabel, alphaRenameRec(r.getBlock).asInstanceOf[Block])
      case rec: Recursion if rec.getLabel == oldLabel => createRecursion(newLabel)
      case c: Choice => createChoice(c, (c.getWhens.asScala map (w => createWhen(w.getMessageSignature, alphaRenameRec(w.getBlock).asInstanceOf[Block]))))
      case b: Block => createBlock(b.getContents.asScala.map(alphaRenameRec(_)))
      case other => other
    }

    acts.map(alphaRenameRec(_))
  }
}