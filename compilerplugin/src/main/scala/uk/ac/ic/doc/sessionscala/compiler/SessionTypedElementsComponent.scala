package uk.ac.ic.doc.sessionscala.compiler

import tools.nsc.Global
import org.scribble.protocol.model.{Role, Activity, RecBlock, ProtocolModel}

/**
 * Created by: omp08
 */

trait SessionTypedElementsComponent {
  self: ScribbleCompilerUtils =>
  val global: Global
  import global._

  type Addresses = Map[Name, (ProtocolModel, Role)]
  type Sessions = Map[Name, Session]
  type LAA = List[(Activity, Activity)]
  type Inferred = Map[Symbol, InferredMethod]

  // todo: refactor inferred into one subclass and sessions into the other. STE should be a trait
  val EmptySTE = new SessionTypedElements
  case class SessionTypedElements(addresses: Addresses,
                                  sessions: Sessions,
                                  inferred: Inferred,
                                  labels: Map[String, (Symbol, Int)])
  {
    def this() = this(Map(), Map(), Map(), Map())
    def updated(sessChan: Name, newSess: Session): SessionTypedElements =
      copy(sessions = sessions.updated(sessChan, newSess))

    def withAddress(sharedChan: Name, model: ProtocolModel, role: Role): SessionTypedElements =
      copy(addresses = addresses.updated(sharedChan, (model,role)))

    def updated(newSess: Sessions) = copy(sessions = newSess)

    def updated(method: Symbol, inf: InferredMethod): SessionTypedElements =
      copy(inferred = inferred.updated(method, inf))

    def addressOfName(name: Name) = addresses.get(name)

    def getInferredFor(method: Symbol, chan: Name): LA =
      getInferredFor(method).getOrElse(chan, Nil)
    def getInferredFor(method: Symbol): InferredMethod =
      inferred.getOrElse(method, InferredMethod())
    def getInferred(method: Symbol, rank: Int): RecBlock = {
      val infMethod = getInferredFor(method)
      infMethod.get(rank) match {
        case Some(l) => l(0).asInstanceOf[RecBlock]
        case None => throw new IllegalArgumentException("No inferred session type known for: "
                + method + " with channel position: " + rank)
      }
    }

    def createInferred(method: Symbol, chan: Name, index: Int) =
      updated(method, getInferredFor(method).add(chan, index))

    def append(method: Symbol, chan: Name, act: Activity) =
      appendAll(method, chan, List(act))
    def appendAll(method: Symbol, chan: Name, acts: LA) =
      updated(method, chan, getInferredFor(method, chan) ::: acts)
    def updated(method: Symbol, chan: Name, inferred: LA): SessionTypedElements =
      updated(method, getInferredFor(method).updated(chan, inferred))

    def inferredFor(label: String) = {
      val (method, rank) = labels(label)
      getInferred(method, rank)
    }

    def registerCompletedMethod(method: Symbol, chans: List[Name], returnedChans: List[Name]) = {
      val newSte = (chans foldLeft this) { case (ste, chan) =>
        val rank = getInferredFor(method).chanToRank(chan)
        val (newSte, label) = ste.ensureMethodParamLabelExists(method, rank)
        newSte.wrapInRecBlock(method, chan, label)
      }
      newSte.recordChanReturnOrder(method, returnedChans)
    }
    def wrapInRecBlock(method: Symbol, chan: Name, label: String) = {
      val listInf = getInferredFor(method, chan)
      updated(method, chan, List(createRecBlock(label, listInf)))
    }
    def ensureMethodParamLabelExists(method: Symbol, rank: Int): (SessionTypedElements, String) = {
      val infMeth = getInferredFor(method)
      var found: String = null
      for ((label, (m,r)) <- labels if found == null)
        if (m == method && r == rank) found = label
      if (found == null) {
        val l = newLabel(rank)
        (copy(labels = labels + (l -> (method,rank))), l)
      } else (this, found)
    }
    def newLabel(rank: Int) = {
      var i = 1
      def value = "X"+i+"c"+rank
      while (labels.get(value).isDefined) i += 1
      //println("newLabel: "+ value)
      value
    }
    def recordChanReturnOrder(method: Symbol, returnedChans: List[Name]) = {
      val inf = getInferredFor(method)
      updated(method, inf.recordChanReturnOrder(returnedChans))
    }

    def clearAllButLabelsAndChanRanks = EmptySTE.copy(labels = labels, inferred = setAllToEmptyList(inferred))

    def hasSessionChannel(name: Name): Boolean = {
      sessions.isDefinedAt(name)
    }

    def removeSessions(chans: Set[Name]) = copy(sessions = sessions -- chans)
  }

  def setAllToEmptyList(inferred: Inferred): Inferred =
    inferred mapValues (infMethod => infMethod.setAllToEmptyList)
  

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
    def get(chan: Name) = chansToInferred.get(chan)
    def getOrElse(chan: Name, default: LA): LA = chansToInferred.getOrElse(chan, default)
    def add(chan: Name, index: Int) = {
      InferredMethod(rankToInferred + (index -> Nil), chansToInferred + (chan -> Nil),
                     chanToRank + (chan -> index), rankToReturned)
    }
    def addWithInferred(chan: Name, index: Int, inf: LA) = add(chan, index).updated(chan, inf)
    def updated(chan: Name, inf: LA): InferredMethod = {
      copy(rankToInferred = rankToInferred + (chanToRank(chan) -> inf),
           chansToInferred = chansToInferred + (chan -> inf))
    }
    def channels = chansToInferred.keys
    def mapValues(f: LA => LA) = {
      val newRTI = rankToInferred.map { case (rank, l) => (rank, f(l)) }
      val newCTI = chansToInferred.map { case (chan, l) => (chan, newRTI(chanToRank(chan))) }
      copy(rankToInferred = newRTI, chansToInferred = newCTI)
    }
    def returnRank(paramRank: Int) = rankToReturned.get(paramRank)
    def recordChanReturnOrder(returnedChans: List[Name]): InferredMethod = {
      var retWithIndex = returnedChans zipWithIndex
      val map = (retWithIndex foldLeft Map[Int, Int]()) { case (result, (retChan, index)) =>
        result + (chanToRank(retChan) -> index)
      }
      copy(rankToReturned = map)
    }
    def setAllToEmptyList = {
      def empty(x: LA): LA = Nil
      copy(rankToInferred = rankToInferred mapValues empty,
           chansToInferred = chansToInferred mapValues empty)
    }
  }  
}