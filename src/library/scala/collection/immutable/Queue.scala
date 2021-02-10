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

package scala.collection
package immutable

import scala.collection.generic.DefaultSerializable
import scala.collection.mutable.{Builder, ListBuffer}

/** `Queue` objects implement data structures that allow to
  *  insert and retrieve elements in a first-in-first-out (FIFO) manner.
  *
  *  `Queue` is implemented as a pair of `List`s, one containing the ''in'' elements and the other the ''out'' elements.
  *  Elements are added to the ''in'' list and removed from the ''out'' list. When the ''out'' list runs dry, the
  *  queue is pivoted by replacing the ''out'' list by ''in.reverse'', and ''in'' by ''Nil''.
  *
  *  Adding items to the queue always has cost `O(1)`. Removing items has cost `O(1)`, except in the case
  *  where a pivot is required, in which case, a cost of `O(n)` is incurred, where `n` is the number of elements in the queue. When this happens,
  *  `n` remove operations with `O(1)` cost are guaranteed. Removing an item is on average `O(1)`.
  *
  *  @see [[https://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#immutable-queues "Scala's Collection Library overview"]]
  *  section on `Immutable Queues` for more information.
  *
  *  @define Coll `immutable.Queue`
  *  @define coll immutable queue
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */

sealed class Queue[+A] protected(protected val in: List[Any /* A | Many[A] */], protected val out: List[A])
  extends AbstractSeq[A]
    with LinearSeq[A]
    with LinearSeqOps[A, Queue, Queue[A]]
    with StrictOptimizedLinearSeqOps[A, Queue, Queue[A]]
    with StrictOptimizedSeqOps[A, Queue, Queue[A]]
    with IterableFactoryDefaults[A, Queue]
    with DefaultSerializable {

  private def inLength(): Int = {
    var result = 0
    var curr = in

    while (curr.nonEmpty) {
      in.head match {
        case m: Queue.Many[_] =>
          result += m.underlying.length
        case _                =>
          result += 1
      }
      curr = curr.tail
    }
    result
  }

  private def inApply(n: Int, originalN: Int): A = {
    def indexOutOfRange(): Nothing = throw new IndexOutOfBoundsException(originalN.toString)
    var curr = in
    var remaining = n
    while (curr.nonEmpty) {
      in.head match {
        case m: Queue.Many[_] =>
          val mLen = m.underlying.length
          if (mLen <= remaining) {
            remaining -= mLen
          } else {
            return m.underlying.apply(mLen - remaining - 1).asInstanceOf[A]
          }
        case other =>
          if (remaining == 0) {
            return other.asInstanceOf[A]
          }
          remaining -= 1
      }
      curr = curr.tail
    }
    ???
  }

  private def inToOut(): List[A] = {
    var curr = in
    val lb = ListBuffer[A]()
    while (curr.nonEmpty) {
      curr.head match {
        case m: Queue.Many[A] =>
          lb.prependAll(m.underlying.asInstanceOf[Seq[A]])
        case other =>
          lb.prepend(other.asInstanceOf[A])
      }
      curr = curr.tail
    }
    lb.result()
  }

  override def iterableFactory: SeqFactory[Queue] = Queue

  /** Returns the `n`-th element of this queue.
    *  The first element is at position `0`.
    *
    *  @param  n index of the element to return
    *  @return   the element at position `n` in this queue.
    *  @throws NoSuchElementException if the queue is too short.
    */
  override def apply(n: Int): A = {
    def indexOutOfRange(): Nothing = throw new IndexOutOfBoundsException(n.toString)

    var index = 0
    var curr = out

    while (index < n && curr.nonEmpty) {
      index += 1
      curr = curr.tail
    }

    if (index == n) {
      if (curr.nonEmpty) curr.head
      else if (in.nonEmpty) {
        in.last match {
          case many: Queue.Many[A] =>
            many.underlying.last
          case other =>
            other.asInstanceOf[A]
        }
      }
      else indexOutOfRange()
    } else {
      val indexFromBack = n - index
      val _inLength = inLength()
      if (indexFromBack >= _inLength) indexOutOfRange()
      else inApply(_inLength - indexFromBack - 1, n)
    }
  }

  /** Returns the elements in the list as an iterator
    */
  override def iterator: Iterator[A] = out.iterator.concat(in.reverseIterator.flatMap {
    case m: Queue.Many[A] => m.underlying.iterator
    case a: A => Iterator.single(a)
  })

  /** Checks if the queue is empty.
    *
    *  @return true, iff there is no element in the queue.
    */
  override def isEmpty: Boolean = in.isEmpty && out.isEmpty

  override def head: A =
    if (out.nonEmpty) out.head
    else if (in.nonEmpty) in.last match {
      case m: Queue.Many[_] =>
        m.underlying.last.asInstanceOf[A]
      case other =>
        other.asInstanceOf[A]
    }
    else throw new NoSuchElementException("head on empty queue")

  override def tail: Queue[A] =
    if (out.nonEmpty) new Queue(in, out.tail)
    else if (in.nonEmpty) {
      new Queue(Nil, inToOut().tail)
    }
    else throw new NoSuchElementException("tail on empty queue")

  /* This is made to avoid inefficient implementation of iterator. */
  override def forall(p: A => Boolean): Boolean =
    in.forall {
      case m: Queue.Many[_] =>
        m.underlying.asInstanceOf[Seq[A]].forall(p)
      case other =>
        p(other.asInstanceOf[A])
    } && out.forall(p)

  /* This is made to avoid inefficient implementation of iterator. */
  override def exists(p: A => Boolean): Boolean =
    in.exists{
      case m: Queue.Many[A] =>
        m.underlying.asInstanceOf[Seq[A]].exists(p)
      case other =>
        p(other.asInstanceOf[A])
    } || out.exists(p)

  override protected[this] def className = "Queue"

  /** Returns the length of the queue. */
  override def length: Int = in.length + out.length

  override def prepended[B >: A](elem: B): Queue[B] = new Queue(in, elem :: out)

  override def appended[B >: A](elem: B): Queue[B] = enqueue(elem)

  override def appendedAll[B >: A](that: scala.collection.IterableOnce[B]): Queue[B] = {
    if (this.isEmpty) {
      Queue.from(that)
    } else {
      val newIn = that match {
        case that: Queue[B] => that.in ++ (that.out reverse_::: this.in)
        case head :: Nil =>
          head :: this.in
        case that: Seq[A] if that.isInstanceOf[List[A]] || that.isInstanceOf[Vector[A]] || that.isInstanceOf[ArraySeq[A]] =>
          if (that.isEmpty) this.in else new Queue.Many(that) :: this.in
        case _ =>
          var result: List[Any] = this.in
          val iter = that.iterator
          while (iter.hasNext) {
            result = iter.next() :: result
          }
          result
      }
      if (newIn eq this.in) this else new Queue[B](newIn, this.out)
    }
  }

  /** Creates a new queue with element added at the end
    *  of the old queue.
    *
    *  @param  elem        the element to insert
    */
  def enqueue[B >: A](elem: B): Queue[B] = new Queue(elem :: in, out)

  /** Creates a new queue with all elements provided by an `Iterable` object
    *  added at the end of the old queue.
    *
    *  The elements are appended in the order they are given out by the
    *  iterator.
    *
    *  @param  iter        an iterable object
    */
  @deprecated("Use `enqueueAll` instead of `enqueue` to enqueue a collection of elements", "2.13.0")
  @`inline` final def enqueue[B >: A](iter: scala.collection.Iterable[B]) = enqueueAll(iter)

  /** Creates a new queue with all elements provided by an `Iterable` object
    *  added at the end of the old queue.
    *
    *  The elements are appended in the order they are given out by the
    *  iterator.
    *
    *  @param  iter        an iterable object
    */
  def enqueueAll[B >: A](iter: scala.collection.Iterable[B]): Queue[B] = appendedAll(iter)

  /** Returns a tuple with the first element in the queue,
    *  and a new queue with this element removed.
    *
    *  @throws NoSuchElementException
    *  @return the first element of the queue.
    */
  def dequeue: (A, Queue[A]) = out match {
    case Nil if !in.isEmpty =>
      val rev = inToOut() ;
      (rev.head, new Queue(Nil, rev.tail))
    case x :: xs            => (x, new Queue(in, xs))
    case _                  => throw new NoSuchElementException("dequeue on empty queue")
  }

  /** Optionally retrieves the first element and a queue of the remaining elements.
    *
    * @return A tuple of the first element of the queue, and a new queue with this element removed.
    *         If the queue is empty, `None` is returned.
    */
  def dequeueOption: Option[(A, Queue[A])] = if(isEmpty) None else Some(dequeue)

  /** Returns the first element in the queue, or throws an error if there
    *  is no element contained in the queue.
    *
    *  @throws NoSuchElementException
    *  @return the first element.
    */
  def front: A = head

  /** Returns a string representation of this queue.
    */
  override def toString(): String = mkString("Queue(", ", ", ")")
}

/** $factoryInfo
  *  @define Coll `immutable.Queue`
  *  @define coll immutable queue
  */
@SerialVersionUID(3L)
object Queue extends StrictOptimizedSeqFactory[Queue] {
  def newBuilder[A]: Builder[A, Queue[A]] = new ListBuffer[A] mapResult (x => new Queue[A](Nil, x))

  def from[A](source: IterableOnce[A]): Queue[A] = source match {
    case q: Queue[A] => q
    case _ =>
      val list = List.from(source)
      if (list.isEmpty) empty
      else new Queue(Nil, list)
  }

  def empty[A]: Queue[A] = EmptyQueue
  override def apply[A](xs: A*): Queue[A] = new Queue[A](Nil, xs.toList)

  private object EmptyQueue extends Queue[Nothing](Nil, Nil) { }

  private final class Many[+A](val underlying: Seq[A] /* List[A] | Vector[A] | ArraySeq[A] */)
}
