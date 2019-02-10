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


import mutable.Builder
import scala.annotation.tailrec
import scala.collection.generic.DefaultSerializable

/**
  * This class implements immutable sets using a list-based data structure. List set iterators and
  * traversal methods visit elements in the order they were first inserted.
  *
  * Elements are stored internally in reversed insertion order, which means the newest element is at
  * the head of the list. As such, methods such as `head` and `tail` are O(n), while `last` and
  * `init` are O(1). Other operations, such as inserting or removing entries, are also O(n), which
  * makes this collection suitable only for a small number of elements.
  *
  * Instances of `ListSet` represent empty sets; they can be either created by calling the
  * constructor directly, or by applying the function `ListSet.empty`.
  *
  * @tparam A the type of the elements contained in this list set
  *
  * @author Matthias Zenger
  * @since 1
  * @define Coll ListSet
  * @define coll list set
  * @define mayNotTerminateInf
  * @define willNotTerminateInf
  */
sealed abstract class ListSet[A]
  extends AbstractSet[A]
    with StrictOptimizedSetOps[A, ListSet, ListSet[A]]
    with DefaultSerializable {

  override protected[this] def className: String = "ListSet"

  override def iterableFactory: IterableFactory[ListSet] = ListSet

  protected def elem: A
  protected def next: ListSet[A]
}

/**
  * $factoryInfo
  *
  * Note that each element insertion takes O(n) time, which means that creating a list set with
  * n elements will take O(n^2^) time. This makes the builder suitable only for a small number of
  * elements.
  *
  * @since 1
  * @define Coll ListSet
  * @define coll list set
  */
@SerialVersionUID(3L)
object ListSet extends IterableFactory[ListSet] {
  private object EmptyListSet extends ListSet[Any] {
    override def size: Int = 0
    override def knownSize: Int = 0
    override def isEmpty: Boolean = true

    def contains(elem: Any): Boolean = false

    def incl(elem: Any): ListSet[Any] = new ListSet.Node(elem, this)
    def excl(elem: Any): ListSet[Any] = this

    override def iterator: Iterator[Any] = Iterator.empty

    protected def elem: Any = throw new NoSuchElementException("elem of empty set")
    protected def next: ListSet[Any] = throw new NoSuchElementException("next of empty set")
  }

  private[immutable] final class Node[A](
    override protected val elem: A,
    private[immutable] var _init: ListSet[A]) extends ListSet[A] {

    override def size = sizeInternal(this, 0)
    override def knownSize: Int = -1
    @tailrec private[this] def sizeInternal(n: ListSet[A], acc: Int): Int =
      if (n.isEmpty) acc
      else sizeInternal(n.next, acc + 1)

    override def isEmpty: Boolean = false

    override def contains(e: A) = containsInternal(this, e)

    @tailrec private[this] def containsInternal(n: ListSet[A], e: A): Boolean =
      !n.isEmpty && (n.elem == e || containsInternal(n.next, e))

    override def incl(e: A): ListSet[A] = if (contains(e)) this else new Node(e, this)

    override def excl(e: A): ListSet[A] = removeInternal(e, this, Nil)

    @tailrec private[this] def removeInternal(k: A, cur: ListSet[A], acc: List[ListSet[A]]): ListSet[A] =
      if (cur.isEmpty) acc.last
      else if (k == cur.elem) acc.foldLeft(cur.next)((t, h) => new Node(h.elem, t))
      else removeInternal(k, cur.next, cur :: acc)

    override protected def next: ListSet[A] = _init

    override def last: A = elem

    override def init: ListSet[A] = next

    override def iterator: scala.collection.Iterator[A] = {
      var curr: ListSet[A] = this
      var res: List[A] = Nil
      while (!curr.isEmpty) {
        res = curr.elem :: res
        curr = curr.next
      }
      res.iterator
    }
  }

  def from[E](it: scala.collection.IterableOnce[E]): ListSet[E] =
    it match {
      case ls: ListSet[E] => ls
      case _ if it.knownSize == 0 => empty[E]
      case _: collection.Map[_, _] | _: MapView[_, _] | _: collection.Set[E] =>
        val iter = it.iterator
        var curr: ListSet[E] = empty
        while (iter.hasNext) {
          curr = new Node(iter.next(), curr)
        }
        curr
      case _ => (newBuilder[E] ++= it).result()
    }

  def empty[A]: ListSet[A] = EmptyListSet.asInstanceOf[ListSet[A]]

  def newBuilder[A]: Builder[A, ListSet[A]] = new ListSetBuilder[A]
}

private[immutable] final class ListSetBuilder[A] extends Builder[A, ListSet[A]] {
  private[this] var underlying = ListSet.empty[A]

  override def clear(): Unit = underlying = ListSet.empty[A]

  override def result(): ListSet[A] = underlying

  override def addOne(elem: A): this.type = {
    underlying = underlying.incl(elem)
    this
  }

  override def addAll(xs: IterableOnce[A]): this.type = {
    xs match {
      case _ if underlying.isEmpty =>
        underlying = ListSet.from(xs)
      case _: collection.Map[_, _] | _: MapView[_, _] | _: collection.Set[A] =>
        val iter = xs.iterator
        var newUnderlying: ListSet[A] = underlying
        while (iter.hasNext) {
          val next = iter.next()
          if (!underlying.contains(next)) {
            newUnderlying = new ListSet.Node(next, newUnderlying)
          }
        }
        underlying = newUnderlying
      case _ =>
        val iter = xs.iterator
        while (iter.hasNext) {
          val next = iter.next()

          if (!underlying.contains(next)) {
            underlying = new ListSet.Node(next, underlying)
          }
        }
    }
    this
  }
}
