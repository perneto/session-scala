package uk.ac.ic.doc.sessionscala.compiler

import org.scribble.protocol.model.{Activity, LabelledBlock, ProtocolModel}
import tools.nsc.Global

/**
 * Created by: omp08
 */

trait SessionTypedElementsComponent {
  self: ScribbleModelFactories =>
  val global: Global
  import global._

  type SharedChannels = Map[Name, ProtocolModel]
  type Sessions = Map[Name, Session]
  type LAA = List[(Activity, Activity)]
  type InferredMethod = Map[Name, LA]
  type Inferred = Map[Symbol, InferredMethod]

  // todo: refactor inferred into one subclass and sessions into the other. STE should be a trait
  val EmptySTE = new SessionTypedElements
  case class SessionTypedElements(sharedChannels: SharedChannels, sessions: Sessions, inferred: Inferred, labels: Map[String, Symbol]) {
    def this() = this(Map(), Map(), Map(), Map())
    def updated(sessChan: Name, newSess: Session): SessionTypedElements =
      copy(sessions = sessions.updated(sessChan, newSess))

    def updated(sharedChan: Name, model: ProtocolModel): SessionTypedElements =
      copy(sharedChannels = sharedChannels.updated(sharedChan, model))

    def updated(newSess: Sessions) = copy(sessions = newSess)

    def updated(method: Symbol, inf: Map[Name, LA]): SessionTypedElements =
      copy(inferred = inferred.updated(method, inf))

    def getSharedChan(name: Name) = sharedChannels.get(name)

    def getInferredFor(method: Symbol, chan: Name): LA =
      getInferredFor(method).getOrElse(chan, Nil)
    def getInferredFor(method: Symbol): Map[Name, LA] =
      inferred.getOrElse(method, Map())
    def getInferred(method: Symbol, chan: Name): Option[LabelledBlock] =
      getInferredFor(method).get(chan).map(l => l(0).asInstanceOf[LabelledBlock])

    def createInferred(method: Symbol, chan: Name): SessionTypedElements =
      updated(method, getInferredFor(method) + (chan -> Nil))

    def append(method: Symbol, chan: Name, act: Activity) =
      appendAll(method, chan, List(act))
    def appendAll(method: Symbol, chan: Name, acts: LA) =
      updated(method, chan, getInferredFor(method, chan) ::: acts)
    def updated(method: Symbol, chan: Name, inferred: LA): SessionTypedElements =
      updated(method, getInferredFor(method).updated(chan, inferred))
    def dropChan(method: Symbol, chan: Name) =
      updated(method, getInferredFor(method) - chan)

    def methodFor(label: String) = labels.get(label)
    def registerMethod(method: Symbol): (SessionTypedElements, String) = {
      var found: String = null
      for ((label, m) <- labels if found == null)
        if (m == method) found = label
      if (found == null) {
        val l = newLabel()
        (copy(labels = labels + (l -> method)), l)
      } else (this, found)
    }
    def newLabel() = {
      var i = 1
      while (labels.get("X"+i).isDefined) i += 1
      println("newLabel: X" + i)
      "X"+i
    }
    def clearAllButLabels = EmptySTE.copy(labels = labels)
  }
}