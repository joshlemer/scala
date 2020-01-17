/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection.immutable

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{Hashing, MapFactory, MapFactoryDefaults}
import scala.collection.immutable.{LinkedHashMap => LHM}


/** an immutable SeqMap implementation that is similar to a mutable LinkedHashMap
 *
 * Keys are inserted in an immutable HashMap, inside a node (Link), which refers forwards by reference to the next
 * Link, and backwards by LOOKUP KEY to the previous Link.
 *
 * To have acceptable updates/removals, the entire Map is not forward-linked by reference. The forward-reference chain
 * breaks at regular intervals of `arity` nodes, where `arity` is a configurable parameter in the companion object.
 *
 * For instance, with `arity`= 3, we would have
 *
 * LinkedHashMap("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4, "e" -> 5", "f" -> 6, "g" -> 7)
 *
 * represented as:
 *
 *   ("a",1) ===> ("b",2) ===> ("c",3") -> ("d",4) ===> ("e",5) ===> ("f",6) -> ("g",7)
 *            <-           <-           <-          <-           <-          <-
 *
 *
 *   legend: ===> is a reference by memory address
 *           ->   is a forward reference by KEY (via lookup in an immutable HashMap)
 *           <-   is a backward reference by KEY (via lookup in an immutable HashMap)
 *
 *
 *
 * Performance Characteristics:
 *
 *   Construction: Extremely fast (fastest of all immutable SeqMaps)! O(n log n). Approximately as fast as constructing
 *   a HashMap, with minimal overhead involved in wrapping values in `Link[K, V]` instances and mutating prev/next
 *   values during building.
 *
 *   Traversal/Iteration: Extremely fast (fastest of all immutable SeqMaps)! O(n + (1/arity) * n log n).
 *   Almost as fast as traversing a `List`, but every `arity` elements, a hashmap lookup is required. For reference,
 *   each lookup is approximately 25 ns.
 *
 *   Lookup: Extremely fast (on par with other immutable SeqMaps)! O(log n). Simply a lookup in a
 *   `HashMap[K, Link[K, V]]`, and then an additional dereferencing of the value. Virtually as fast as HashMap itself.
 *
 *   Updates: Quite slow, but not pathological (1/3 as fast as VectorMap)! O((arity/2) * log n). When updating a key, all nodes
 *   that PRECEDE that node, in its `arity`-sized segment must be updated accordingly. Any nodes occurring after
 *   the node only refer backwards by key, not by reference, so they need not be adjusted. So an update is equivalent to
 *   roughly arity/2 lookups and arity/2 updates. The shallow mutation capabilities of immutable.HashMap are however
 *   utilized to mitigate the downside.
 *
 */
final class LinkedHashMap[K, +V] private (private val _first: LHM.Link[K, V], private val _last: LHM.Link[K, V], hm: HashMap[K, LHM.Link[K, V]])
  extends AbstractMap[K, V]
    with SeqMap[K, V]
    with StrictOptimizedMapOps[K, V, LinkedHashMap, LinkedHashMap[K, V]]
    with MapFactoryDefaults[K, V, LinkedHashMap, Iterable] {

  override def mapFactory: MapFactory[LinkedHashMap] = LinkedHashMap

  private[this] def allPreviousLinksInChain(link: LHM.Link[K, V], includeArgument: Boolean): List[LHM.Link[K, V]] = {
    var result: List[LHM.Link[K, V]] = Nil
    if (includeArgument) result = link :: result
    var curr = link
    while(curr.prev.asInstanceOf[AnyRef] ne LHM.End) {
      val prevKey = curr.prev.asInstanceOf[K]
      val prevLink = hm(prevKey)
      if (prevLink.next.asInstanceOf[AnyRef] eq curr) {
        result = prevLink :: result
      } else {
        return result
      }
      curr = prevLink
    }
    result
  }
  private[this] def concatToHm[V1 >: V](list: List[LHM.Link[K, V1]]): HashMap[K, LHM.Link[K, V1]] = {
    var rootNode: BitmapIndexedMapNode[K, LHM.Link[K, V1]] = hm.rootNode
    var curr = list
    while (!curr.isEmpty) {
      val next = curr.head
      val hash = next.key.##
      rootNode = rootNode.updated(next.key, next, hash, Hashing.improve(hash), 0, replaceValue = true)
      curr = curr.tail
      if (rootNode ne hm.rootNode) {
        var nodeMap = 0
        while (!curr.isEmpty) {
          val next = curr.head
          val hash = next.key.##
          nodeMap = rootNode.updateWithShallowMutations(next.key, next, hash, Hashing.improve(hash), 0, nodeMap)
          curr = curr.tail
        }
        return new HashMap(rootNode)
      }
    }
    hm
  }

  override def removed(key: K): LinkedHashMap[K, V] = ???
  override def updated[V1 >: V](key: K, value: V1): LinkedHashMap[K, V1] = {
    hm.get(key) match {
      case Some(old) =>
        if (old.value.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) {
          this
        } else {
          val oldPrevLinks = allPreviousLinksInChain(old, includeArgument = false)

          var newPrevLinks: List[LHM.Link[K, V]] = List.empty[LHM.Link[K, V]]

          {
            var curr = oldPrevLinks
            while (!curr.isEmpty) {
              val link = curr.head
              val copiedLink = link.copy()
              newPrevLinks match {
                case h :: _ =>
                  h.next = copiedLink
                case _ =>
              }
              newPrevLinks = copiedLink :: newPrevLinks
              curr = curr.tail
            }
          }

          val newLink = new LHM.Link(key, value, old.prev, old.next)

          newPrevLinks match {
            case h :: _ => h.next = newLink
            case _ =>
          }

          val allNewLinks = newLink :: newPrevLinks

          val newHm = concatToHm(allNewLinks)

          var newFirst: LHM.Link[K, V1] = _first
          var newLast: LHM.Link[K, V1] = _last

          if (newFirst == old) {
            newFirst = newLink
          } else if (newFirst == null) {
            newFirst = newLink
            newLast = newLink
          } else if(oldPrevLinks.nonEmpty && (oldPrevLinks.head eq _first)) {
            newFirst = newPrevLinks.last
          }

          if (newLast eq old) {
            newLast = newLink
          }
          new LinkedHashMap(newFirst, newLast, newHm)
        }
      case None =>
        if (_first == null) {
          val newLink = new LHM.Link(key, value, LHM.End, LHM.End)
          return new LinkedHashMap(newLink, newLink, HashMap.empty.updated(key, newLink))
        }

        val oldPrevLinks = allPreviousLinksInChain(_last, includeArgument = true)
        val newPrevLinks = oldPrevLinks.foldLeft(List.empty[LHM.Link[K, V]]){
          case (acc @ h :: _, link) =>
            val copiedLink = link.copy()
            h.next = copiedLink
            copiedLink :: acc
          case (Nil, link) =>
            link.copy() :: Nil
        }

        val newPrevLink = newPrevLinks.head
        val newLink = new LHM.Link(key, value, newPrevLink.key, LHM.End)
        newPrevLink.next = if (hm.size % LHM.arity == 0) key else newLink

        val newHm = concatToHm(newLink :: newPrevLinks)

        var newFirst: LHM.Link[K, V1] = _first
        if (oldPrevLinks.head eq _first) {
          newFirst = newPrevLinks.last
        }

        new LinkedHashMap(newFirst, newLink, newHm)
    }
  }
  override def get(key: K): Option[V] = hm.getOrElse(key, LHM.End) match {
    case LHM.End => None
    case v => Some(v).asInstanceOf[Option[V]]
  }
  override def iterator: Iterator[(K, V)] = {
    if (_first == null) {
      Iterator.empty
    } else new Iterator[(K, V)] {
      var curr: LHM.Link[K, V] = _first
      override def hasNext: Boolean = curr != null
      override def next(): (K, V) = {
        if (curr == null) {
          Iterator.empty.next()
        } else {
          val result = curr.tuple
          curr = curr.next match {
            case link: LHM.Link[K, V] => link
            case LHM.End => null
            case key => hm(key.asInstanceOf[K])
          }
          result
        }
      }
    }

  }
}

object LinkedHashMap extends MapFactory[LinkedHashMap] {
  private final def arity: Int = 3
  private final val End: AnyRef = new AnyRef {}
  private final class Link[K, +V](val key: K, var value: V @uncheckedVariance, val prev: Any, var next: Any) {
    def copy(): Link[K, V] = new Link(key, value, prev, next)
    def tuple: (K, V) = (key, value)
  }

  private final val Empty: LinkedHashMap[Nothing, Nothing] = new LinkedHashMap[Nothing, Nothing](null, null, HashMap.empty)

  override def empty[K, V]: LinkedHashMap[K, V] = Empty.asInstanceOf[LinkedHashMap[K, V]]

  override def from[K, V](it: IterableOnce[(K, V)]): LinkedHashMap[K, V] =  newBuilder[K, V].addAll(it).result()

  override def newBuilder[K, V]: collection.mutable.Builder[(K, V), LinkedHashMap[K, V]] =
    new collection.mutable.Builder[(K, V), LinkedHashMap[K, V]] {
      private var size: Int = 0
      private var _first: Link[K, V] = null
      private var _last: Link[K, V] = null
      private var hm = new HashMapBuilder[K, Link[K, V]]

      override def clear(): Unit = {
        _first = null
        _last = null
        hm.clear()
        size = 0
      }

      /** Result collection consisting of all elements appended so far. */
      override def result(): LHM[K, V] = {
        val result = new LinkedHashMap[K, V](_first, _last, hm.result())
        clear()
        result
      }

      override def addOne(elem: (K, V)): this.type = {
        val old = hm.getOrElse(elem._1, null)
        if (old == null) {
          val link = new Link(elem._1, elem._2, if (_last == null) End else _last.key, End)
          if(_last != null) {
            if (size % arity != 0) {
              _last.next = link
            } else {
              _last.next = elem._1
            }
          }
          if (_first == null) {
            _first = link
          }
          hm.addOne(elem._1, link)
          size += 1
          _last = link
        } else {
          old.value = elem._2
        }
        this
      }

      override def addAll(xs: IterableOnce[(K, V)]): this.type = {
        var __first = _first
        var __last = _last
        var __size = size
        val iter = xs.iterator
        while (iter.hasNext) {
          val (k, v) = iter.next()
          val old = hm.getOrElse(k, null)
          if (old == null) {
            val link = new Link(k, v, if (__last == null) End else __last.key, End)
            if(__last != null) {
              if (__size % arity != 0) {
                __last.next = link
              } else {
                __last.next = k
              }
            }
            if (__first == null) {
              __first = link
            }
            hm.addOne(k, link)
            __size += 1
            __last = link
          } else {
            old.value = v
          }
        }
        _first = __first
        _last = __last
        size = __size
        this
      }
    }
}
