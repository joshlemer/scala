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

package scala
package collection
package immutable

import BitSetOps.{LogWL, updateArray}
import mutable.Builder
import scala.annotation.implicitNotFound

/** A class for immutable bitsets.
  *  $bitsetinfo
  *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#immutable-bitsets "Scala's Collection Library overview"]]
  *  section on `Immutable BitSets` for more information.
  *
  *  @define Coll `immutable.BitSet`
  *  @define coll immutable bitset
  */
sealed abstract class BitSet
  extends AbstractSet[Int]
    with SortedSet[Int]
    with SortedSetOps[Int, SortedSet, BitSet]
    with StrictOptimizedIterableOps[Int, Set, BitSet]
    with StrictOptimizedSortedSetOps[Int, SortedSet, BitSet]
    with collection.BitSet
    with collection.BitSetOps[BitSet] {

  override def unsorted: Set[Int] = this

  def bitSetFactory = BitSet

  protected[collection] def fromBitMaskNoCopy(elems: Array[Long]): BitSet = BitSet.fromBitMaskNoCopy(elems)

  def incl(elem: Int): BitSet = {
    require(elem >= 0, "bitset element must be >= 0")
    if (contains(elem)) this
    else {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) | (1L << elem))
    }
  }

  def excl(elem: Int): BitSet = {
    require(elem >= 0, "bitset element must be >= 0")
    if (contains(elem)) {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) & ~(1L << elem))
    } else this
  }

  def oldRemoveAll(that: IterableOnce[Int]) = super.removeAll(that)

  /** Update word at index `idx`; enlarge set if `idx` outside range of set.
    */
  protected def updateWord(idx: Int, w: Long): BitSet

  override def map(f: Int => Int): BitSet = strictOptimizedMap(newSpecificBuilder, f)
  override def map[B](f: Int => B)(implicit @implicitNotFound(collection.BitSet.ordMsg) ev: Ordering[B]): SortedSet[B] =
    super[StrictOptimizedSortedSetOps].map(f)

  override def flatMap(f: Int => IterableOnce[Int]): BitSet = strictOptimizedFlatMap(newSpecificBuilder, f)
  override def flatMap[B](f: Int => IterableOnce[B])(implicit @implicitNotFound(collection.BitSet.ordMsg) ev: Ordering[B]): SortedSet[B] =
    super[StrictOptimizedSortedSetOps].flatMap(f)

  override def collect(pf: PartialFunction[Int, Int]): BitSet = strictOptimizedCollect(newSpecificBuilder, pf)
  override def collect[B](pf: scala.PartialFunction[Int, B])(implicit @implicitNotFound(collection.BitSet.ordMsg) ev: Ordering[B]): SortedSet[B] =
    super[StrictOptimizedSortedSetOps].collect(pf)

  // necessary for disambiguation
  override def zip[B](that: scala.IterableOnce[B])(implicit @implicitNotFound(collection.BitSet.zipOrdMsg) ev: Ordering[(Int, B)]): SortedSet[(Int, B)] =
    super.zip(that)

  override protected[this] def writeReplace(): AnyRef = new BitSet.SerializationProxy(this)
}

/**
  * $factoryInfo
  * @define Coll `immutable.BitSet`
  * @define coll immutable bitset
  */
@SerialVersionUID(3L)
object BitSet extends SpecificIterableFactory[Int, BitSet] {

  def fromSpecific(it: scala.collection.IterableOnce[Int]): BitSet =
    it match {
      case bs: BitSet => bs
      case _          => (newBuilder ++= it).result()
    }

  def empty: BitSet = new BitSet1(0L)

  def newBuilder: Builder[Int, BitSet] =
    mutable.BitSet.newBuilder.mapResult(bs => fromBitMaskNoCopy(bs.elems))

  private def createSmall(a: Long, b: Long): BitSet = if (b == 0L) new BitSet1(a) else new BitSet2(a, b)

  /** A bitset containing all the bits in an array */
  def fromBitMask(elems: Array[Long]): BitSet = {
    val len = elems.length
    if (len == 0) empty
    else if (len == 1) new BitSet1(elems(0))
    else if (len == 2) createSmall(elems(0), elems(1))
    else {
      val a = java.util.Arrays.copyOf(elems, len)
      new BitSetN(a)
    }
  }

  /** A bitset containing all the bits in an array, wrapping the existing
    *  array without copying.
    */
  def fromBitMaskNoCopy(elems: Array[Long]): BitSet = {
    val len = elems.length
    if (len == 0) empty
    else if (len == 1) new BitSet1(elems(0))
    else if (len == 2) createSmall(elems(0), elems(1))
    else new BitSetN(elems)
  }

  class BitSet1(val elems: Long) extends BitSet {
    protected[collection] def nwords = 1
    protected[collection] def word(idx: Int) = if (idx == 0) elems else 0L
    protected[collection] def updateWord(idx: Int, w: Long): BitSet =
      if (idx == 0) new BitSet1(w)
      else if (idx == 1) createSmall(elems, w)
      else fromBitMaskNoCopy(updateArray(Array(elems), idx, w))
  }

  class BitSet2(val elems0: Long, elems1: Long) extends BitSet {
    protected[collection] def nwords = 2
    protected[collection] def word(idx: Int) = if (idx == 0) elems0 else if (idx == 1) elems1 else 0L
    protected[collection] def updateWord(idx: Int, w: Long): BitSet =
      if (idx == 0) new BitSet2(w, elems1)
      else if (idx == 1) createSmall(elems0, w)
      else fromBitMaskNoCopy(updateArray(Array(elems0, elems1), idx, w))
  }

  class BitSetN(val elems: Array[Long]) extends BitSet {
    protected[collection] def nwords = elems.length
    protected[collection] def word(idx: Int) = if (idx < nwords) elems(idx) else 0L
    protected[collection] def updateWord(idx: Int, w: Long): BitSet = fromBitMaskNoCopy(updateArray(elems, idx, w))

    override def removeAll(that: IterableOnce[Int]): BitSet = that match {
      case bs: collection.BitSet =>
//        ???
        val bsnwords = bs.nwords
        val thisnwords = nwords
        if (bsnwords >= thisnwords) {
          // here, we may have opportunity to shrink the size of the array
          // so, track the highest index which is non-zero. That ( + 1 ) will be our new array length
          var i = thisnwords - 1

          var currentWord = 0L

          // if there are never any changes, we can return `this` at the end
          var anyChanges = false

          while (i >= 0 && currentWord == 0L) {
            val oldWord = word(i)
            currentWord = oldWord & ~bs.word(i)
            anyChanges ||= currentWord != oldWord
            i -= 1
          }

          if (i < 0) {
            // all indices >= 0 have had result 0, so the bitset is empty
            BitSet.empty
          } else {

            val minimumNonZeroIndex: Int = i + 1

            while (!anyChanges && i >= 0) {
              val oldWord = word(i)
              currentWord = oldWord & ~bs.word(i)
              anyChanges ||= currentWord != oldWord
              i -= 1
            }

            if (anyChanges) {
              val newArray = elems.take(minimumNonZeroIndex + 1)
              newArray(i + 1) = currentWord
              while (i >= 0) {
                newArray(i) = word(i) & ~bs.word(i)
                i -= 1
              }
              fromBitMaskNoCopy(newArray)
            } else {
              this
            }
          }

        } else {
          // here, there is no opportunity to shrink the array size, no use in tracking highest non-zero index
          // however, we may still share structure (the array) if no changes are made
          var i = bsnwords - 1

          var anyChanges = false
          var currentWord = 0L

          while (i >= 0 && !anyChanges) {
            val oldWord = word(i)
            currentWord = oldWord & ~bs.word(i)
            anyChanges ||= currentWord != oldWord
            i -= 1
          }

          if (anyChanges) {
            val newElems = elems.clone()
            newElems(i + 1) = currentWord

            while (i >= 0) {
              newElems(i) = word(i) & ~bs.word(i)
              i -= 1
            }
            fromBitMaskNoCopy(newElems)
          } else {
            this
          }
        }
      case _ => super.removeAll(that)
    }
  }

  @SerialVersionUID(3L)
  private final class SerializationProxy(coll: BitSet) extends scala.collection.BitSet.SerializationProxy(coll) {
    protected[this] def readResolve(): Any = BitSet.fromBitMaskNoCopy(elems)
  }
}
