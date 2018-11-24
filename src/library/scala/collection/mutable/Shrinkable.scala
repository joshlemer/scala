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
package collection.mutable

import scala.annotation.tailrec
import scala.collection.IterableOnceOps

/** This trait forms part of collections that can be reduced
  *  using a `-=` operator.
  *
  *  @author   Martin Odersky
  *  @since   2.8
  *  @define coll shrinkable collection
  *  @define Coll `Shrinkable`
  */
trait Shrinkable[-A] {

  /** Removes a single element from this $coll.
    *
    *  @param elem  the element to remove.
    *  @return the $coll itself
    */
  def subtractOne(elem: A): this.type

  /** Alias for `subtractOne` */
  @`inline` final def -= (elem: A): this.type = subtractOne(elem)

  /** Removes two or more elements from this $coll.
    *
    *  @param elem1 the first element to remove.
    *  @param elem2 the second element to remove.
    *  @param elems the remaining elements to remove.
    *  @return the $coll itself
    */
  def -= (elem1: A, elem2: A, elems: A*): this.type = {
    this -= elem1
    this -= elem2
    this --= elems
  }

  /** Removes all elements produced by an iterator from this $coll.
    *
    *  @param xs   the iterator producing the elements to remove.
    *  @return the $coll itself
    */
  def subtractAll(xs: collection.IterableOnce[A]): this.type = {
    @tailrec def loop(xs: collection.LinearSeq[A]): Unit = {
      if (xs.nonEmpty) {
        subtractOne(xs.head)
        loop(xs.tail)
      }
    }
    xs match {
      case xs: collection.LinearSeq[A] => loop(xs)
      case xs => xs.iterator.foreach(subtractOne)
    }
    this
  }

  /** Alias for `subtractAll` */
  @`inline` final def --= (xs: collection.IterableOnce[A]): this.type = subtractAll(xs)

}

trait ShrinkableIterable[A, B] extends Iterable[B] with Shrinkable[A] {
  private[this] val DEFAULT_BATCH_SIZE = 5


  def oldSubtractAll(xs: IterableOnce[A]): this.type = {
    super.subtractAll(xs)
  }

  override def subtractAll(xs: IterableOnce[A]): this.type = {
    xs match {
      case xs: collection.LinearSeq[A] =>
        var curr = xs
        while (curr.nonEmpty && knownSize != 0) {
          var i = 0
          while (i < 100 && curr.nonEmpty) {
            subtractOne(curr.head)
            curr = curr.tail
            i += 1
          }
        }
        this

      case _ =>
        val iter = xs.iterator
        while (iter.hasNext && knownSize != 0) {
          var i = 0
          while (i < 100 && iter.hasNext) {
            subtractOne(iter.next())
            i += 1
          }
        }
        this
    }
  }
}