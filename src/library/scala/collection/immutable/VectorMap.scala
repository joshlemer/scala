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

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.VectorMap.Entry

/** This class implements immutable maps using a vector/map-based data structure, which preserves insertion order.
  *
  *  Unlike `ListMap`, `VectorMap` has amortized effectively constant lookup at the expense
  *  of using extra memory and generally lower performance for other operations
  *
  *  @tparam K      the type of the keys contained in this vector map.
  *  @tparam V      the type of the values associated with the keys in this vector map.
  *
  * @version 2.13
  * @since 2.13
  * @define coll immutable vector map
  * @define Coll `immutable.VectorMap`
  */
final class VectorMap[K, +V] private (
    private[immutable] val fields: Vector[Entry[K, V]],
     private[immutable] val underlying: Map[K, Int], dummy: Boolean)
  extends AbstractMap[K, V]
    with SeqMap[K, V]
    with StrictOptimizedMapOps[K, V, VectorMap, VectorMap[K, V]]
    with MapFactoryDefaults[K, V, VectorMap, Iterable] {

  import VectorMap._

  override protected[this] def className: String = "VectorMap"

  private[immutable] def this(fields: Vector[Entry[K, V]], underlying: Map[K, Int]) = {
    this(fields, underlying, false)
  }

  override val size: Int = underlying.size

  override def knownSize: Int = size

  override def isEmpty: Boolean = size == 0

  def updated[V1 >: V](key: K, value: V1): VectorMap[K, V1] =
    underlying.getOrElse(key, -1) match {
      case -1 =>
        new VectorMap(fields :+ new Entry(key, value), underlying.updated(key, fields.length), false)
      case slot =>
        val oldEntry = fields(slot)
        if (value.asInstanceOf[AnyRef] eq oldEntry.value.asInstanceOf[AnyRef]) {
          this
        } else {
          new VectorMap(fields.updated(slot, oldEntry.withValue(value)), underlying, false)
        }
    }

  override def withDefault[V1 >: V](d: K => V1): Map[K, V1] =
    new Map.WithDefault(this, d)

  override def withDefaultValue[V1 >: V](d: V1): Map[K, V1] =
    new Map.WithDefault[K, V1](this, _ => d)

  def get(key: K): Option[V] = underlying.get(key) match {
    case Some(e) => Some(fields(e).value)
    case None    => None
  }

  def iterator: Iterator[(K, V)] = fields.iterator.collect {
    case entry if entry != null => (entry.key, entry.value)
  }

  // No-Op overrides to allow for more efficient steppers in a minor release.
  // Refining the return type to `S with EfficientSplit` is binary compatible.

  override def stepper[S <: Stepper[_]](implicit shape: StepperShape[(K, V), S]): S = super.stepper(shape)

  override def keyStepper[S <: Stepper[_]](implicit shape: StepperShape[K, S]): S = super.keyStepper(shape)

  override def valueStepper[S <: Stepper[_]](implicit shape: StepperShape[V, S]): S = super.valueStepper(shape)


  def removed(key: K): VectorMap[K, V] = {
    if (isEmpty) empty
    else {
      var slot = -1
      val newUnderlying = underlying.updatedWith(key) { opt =>
        if (opt.isDefined) slot = opt.get
        None
      }

      if (slot == -1) this else {
        val newFields = if (slot == fields.length - 1) fields.init else fields.updated(slot, null)
        new VectorMap[K, V](newFields, newUnderlying)
      }
    }
  }

  override def mapFactory: MapFactory[VectorMap] = VectorMap

  override def contains(key: K): Boolean = underlying.contains(key)

  override def head: (K, V) = iterator.next()

  override def last: (K, V) = lastOption.get

  override def lastOption: Option[(K, V)] =
    fields
      .reverseIterator
      .collectFirst { case e if e != null => e.toTuple}

  override def tail: VectorMap[K, V] = {
    if (size == 0) throw new NoSuchElementException("tail of empty VectorMap")
    else if (size == 1) empty
    else {
      val iter = fields.iterator
      var ind = 0
      while (iter.hasNext) {
        val next = iter.next()
        if (next != null) {
          return new VectorMap(fields.updated(ind, null), underlying.removed(next.key))
        }
        ind += 1
      }
      throw new Exception
    }
  }

  override def init: VectorMap[K, V] = {
    if (size == 0) throw new NoSuchElementException("tail of empty VectorMap")
    else if (size == 1) empty
    else {
      var newSize = fields.length
      val iter = fields.reverseIterator
      while (iter.hasNext) {
        val next = iter.next()
        if (next != null) {
          return new VectorMap(fields.take(newSize), underlying.removed(next.key))
        }
        newSize -= 1
      }
      ???
    }
  }

  override def keys: Vector[K] = keysIterator.toVector

  override def values: Iterable[V] = new Iterable[V] with IterableFactoryDefaults[V, Iterable] {
    override def iterator: Iterator[V] = fields.iterator.collect { case e if e != null => e.value }
  }
}

object VectorMap extends MapFactory[VectorMap] {

  private [immutable] final class Entry[K, +V](val key: K, var value: V @uncheckedVariance) {
    def toTuple: (K, V) = (key, value)
    def withValue[V1 >: V](v: V1): Entry[K, V1] = new Entry(key, v)
  }

  private[this] final val EmptyMap: VectorMap[Nothing, Nothing] = new VectorMap[Nothing, Nothing](Vector.empty, HashMap.empty)

  def empty[K, V]: VectorMap[K, V] = EmptyMap.asInstanceOf[VectorMap[K, V]]

  def from[K, V](it: collection.IterableOnce[(K, V)]): VectorMap[K, V] =
    it match {
      case vm: VectorMap[K, V] => vm
      case _                   => (newBuilder[K, V] ++= it).result()
    }

  def newBuilder[K, V]: mutable.Builder[(K, V), VectorMap[K, V]] = new VectorMapBuilder[K, V]
}

private[immutable] final class VectorMapBuilder[K, V] extends mutable.Builder[(K, V), VectorMap[K, V]] {
  private[this] val vectorBuilder = new VectorBuilder[Entry[K, V]]
  private[this] val mapBuilder = new MapBuilderImpl[K, Int]
  private[this] var aliased: VectorMap[K, V] = _

  override def clear(): Unit = {
    vectorBuilder.clear()
    mapBuilder.clear()
    aliased = null
  }

  override def result(): VectorMap[K, V] = {
    if (aliased eq null) {
        aliased = new VectorMap(vectorBuilder.result(), mapBuilder.result())
    }
    aliased
  }
  def addOne(key: K, value: V): this.type = {
    if (aliased ne null) {
      aliased = aliased.updated(key, value)
    } else {
      mapBuilder.getOrElse(key, -1) match {
        case -1 =>
          val vectorSize = vectorBuilder.size
          vectorBuilder.addOne(new Entry(key, value))
          mapBuilder.addOne(key, vectorSize)
        case slot =>
          vectorBuilder(slot).value = value
      }
    }
    this
  }

  override def addOne(elem: (K, V)): this.type = addOne(elem._1, elem._2)
}
