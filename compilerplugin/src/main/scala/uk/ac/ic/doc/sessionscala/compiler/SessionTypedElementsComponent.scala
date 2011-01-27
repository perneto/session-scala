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
  type Inferred = Map[Symbol, InferredMethod]


  def contained[V](seq1: Iterable[V], seq2: Iterable[V]): Boolean = {
    val seq2_ = Nil ++ seq2
    (seq1 foldLeft true) { case (result, value) =>
      seq2_.contains(value)
    }
  }

  def equal[V](seq1: Iterable[V], seq2: Iterable[V]) = contained(seq1, seq2) && contained(seq2, seq1)

  def valuesEq[K1, K2, V](map1: Map[K1, V], map2: Map[K2, V]): Boolean =
    equal(map1.values, map2.values) && equal(map2.values, map1.values)


  object InferredMethod {
    def apply() = new InferredMethod(Map(), Map(), Map(), Map())
  }
  case class InferredMethod(rankToInferred: Map[Int, LA], chansToInferred: Map[Name, LA],
                            chanToRank: Map[Name, Int], rankToReturned: Map[Int, Int])
  {
    assert(valuesEq(rankToInferred, chansToInferred)
            && chanToRank.keys == chansToInferred.keys
            && equal(chanToRank.values, rankToInferred.keys),
      "Inconsistent InferredMethod: " + rankToInferred + ", " + chansToInferred + ", " + chanToRank)
    def get(rank: Int) = rankToInferred.get(rank)
    def rankMax = rankToInferred.keys.max
    def get(chan: Name) = chansToInferred.get(chan)
    def getOrElse(chan: Name, default: LA): LA = chansToInferred.getOrElse(chan, default)
    def add(chan: Name) = {
      // todo: ensure that this method gets called in the order of the parameters (it's the case now but easy to break)
      val i = if (rankToInferred.size == 0) 0
      else rankToInferred.keys.max + 1
      InferredMethod(rankToInferred + (i -> Nil), chansToInferred + (chan -> Nil),
                     chanToRank + (chan -> i), rankToReturned)
    }
    def updated(chan: Name, inf: LA): InferredMethod = {
      if (!chanToRank.isDefinedAt(chan)) add(chan).updated(chan, inf)
      else copy(rankToInferred = rankToInferred + (chanToRank(chan) -> inf),
              chansToInferred = chansToInferred + (chan -> inf))
    }
    def channels = chansToInferred.keys
    def mapValues(f: LA => LA) = {
      val newRTI = rankToInferred.map { case (rank, l) => (rank, f(l)) }
      val newCTI = chansToInferred.map { case (chan, l) => (chan, newRTI(chanToRank(chan))) }
      copy(rankToInferred = newRTI, chansToInferred = newCTI)
    }
    def returnRank(paramRank: Int) = rankToReturned(paramRank)
    def recordChanReturnOrder(returnedChans: List[Name]): InferredMethod = {
      var zipped = returnedChans zipWithIndex
      val map = (zipped foldLeft Map[Int, Int]()) { case (result, (retChan, index)) =>
        result + (chanToRank(retChan) -> index)
      }
      copy(rankToReturned = map)
    }
  }

  // todo: refactor inferred into one subclass and sessions into the other. STE should be a trait
  val EmptySTE = new SessionTypedElements
  case class SessionTypedElements(sharedChannels: SharedChannels, sessions: Sessions, inferred: Inferred, labels: Map[String, Symbol]) {
    def this() = this(Map(), Map(), Map(), Map())
    def updated(sessChan: Name, newSess: Session): SessionTypedElements =
      copy(sessions = sessions.updated(sessChan, newSess))

    def updated(sharedChan: Name, model: ProtocolModel): SessionTypedElements =
      copy(sharedChannels = sharedChannels.updated(sharedChan, model))

    def updated(newSess: Sessions) = copy(sessions = newSess)

    def updated(method: Symbol, inf: InferredMethod): SessionTypedElements =
      copy(inferred = inferred.updated(method, inf))

    def getSharedChan(name: Name) = sharedChannels.get(name)

    def getInferredFor(method: Symbol, chan: Name): LA =
      getInferredFor(method).getOrElse(chan, Nil)
    def getInferredFor(method: Symbol): InferredMethod =
      inferred.getOrElse(method, InferredMethod())
    def getInferred(method: Symbol, rank: Int): LabelledBlock = {
      val infMethod = getInferredFor(method)
      infMethod.get(rank) match {
        case Some(l) => l(0).asInstanceOf[LabelledBlock]
        case None => throw new IllegalArgumentException("No inferred session type known for: "
                + method + " with channel position: " + rank + " (maximum position: " +infMethod.rankMax+ ")")
      }
    }

    def createInferred(method: Symbol, chan: Name, index: Int) =
      updated(method, getInferredFor(method).add(chan))

    def append(method: Symbol, chan: Name, act: Activity) =
      appendAll(method, chan, List(act))
    def appendAll(method: Symbol, chan: Name, acts: LA) =
      updated(method, chan, getInferredFor(method, chan) ::: acts)
    def updated(method: Symbol, chan: Name, inferred: LA): SessionTypedElements =
      updated(method, getInferredFor(method).updated(chan, inferred))

    def methodFor(label: String) = labels.get(label)

    def registerCompletedMethod(method: Symbol, returnedChans: List[Name]) = {
      val (newSte, label) = ensureMethodLabelExists(method)
      val newSte2 = newSte.wrapInLabelledBlock(method, label)
      newSte2.recordChanReturnOrder(method, returnedChans)
    }
    def wrapInLabelledBlock(method: Symbol, label: String) = {
      val inf = getInferredFor(method) mapValues {
        listInf => List(createLabelledBlock(label, listInf))
      }
      updated(method, inf)
    }
    def ensureMethodLabelExists(method: Symbol): (SessionTypedElements, String) = {
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
    def recordChanReturnOrder(method: Symbol, returnedChans: List[Name]) = {
      val inf = getInferredFor(method)
      updated(method, inf.recordChanReturnOrder(returnedChans))
    }

    def clearAllButLabels = EmptySTE.copy(labels = labels)

    def hasSessionChannel(name: Name): Boolean = {
      sessions.isDefinedAt(name)
    }

    def removeSession(chan: Name) = copy(sessions = sessions - chan)
  }
}