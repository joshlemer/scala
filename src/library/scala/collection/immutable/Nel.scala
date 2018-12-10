package scala.collection.immutable

import scala.collection.mutable
final class Nel[+A](override val head: A, override val tail: List[A])
  extends AbstractSeq[A]
    with LinearSeq[A]
    with LinearSeqOps[A, LinearSeq, LinearSeq[A]]
    with StrictOptimizedSeqOps[A, LinearSeq, LinearSeq[A]] {

  override def toList: List[A] = head :: tail

  private[this] def fromList[A0](list: List[A0]): Nel[A0] = new Nel(list.head, list.tail)

  override def distinctBy[B](f: A => B): Nel[A] = fromList(toList.distinctBy(f))

  override def updated[B >: A](index: Int, elem: B): Nel[B] =
    if (index == 0) new Nel(elem, tail)
    else new Nel(head, tail.updated(index - 1, elem))

  override def prepended[B >: A](elem: B): Nel[B] = new Nel(elem, head :: tail)

  override def appended[B >: A](elem: B): Nel[B] = new Nel(head, tail.appended(elem))

  override def appendedAll[B >: A](suffix: IterableOnce[B]): Nel[B] = new Nel(head, tail.appendedAll(suffix))

  override def prependedAll[B >: A](prefix: IterableOnce[B]): Nel[B] = fromList(toList.prependedAll(prefix))

  override def padTo[B >: A](len: Int, elem: B): Nel[B] = fromList(toList.padTo(len, elem))

  override def unzip[A1, A2](implicit asPair: A => (A1, A2)): (Nel[A1], Nel[A2]) = {
    val (h1, h2) = asPair(head)
    val (t1, t2) = tail.unzip
    (new Nel(h1, t1), new Nel(h2, t2))
  }

  override def unzip3[A1, A2, A3](implicit asTriple: A => (A1, A2, A3)): (Nel[A1], Nel[A2], Nel[A3]) = {
    val (h1, h2, h3) = asTriple(head)
    val (t1, t2, t3) = tail.unzip3
    (new Nel(h1, t1), new Nel(h2, t2), new Nel(h3, t3))
  }

  override def map[B](f: A => B): Nel[B] = new Nel(f(head), tail.map(f))
  override def concat[B >: A](suffix: IterableOnce[B]): Nel[B] = new Nel(head, tail.concat(suffix))
  override def sorted[B >: A](implicit ord: Ordering[B]): Nel[A] = fromList(toList.sorted(ord))
  override def sortWith(lt: (A, A) => Boolean): Nel[A] = fromList(toList.sortWith(lt))
  override def sortBy[B](f: A => B)(implicit ord: Ordering[B]): Nel[A] = fromList(toList.sortBy(f))
  override def isEmpty: Boolean = false
  override def className: String = "Nel"
  override def iterator: Iterator[A] = toList.iterator
  override def reverse: Nel[A] = fromList(toList.reverse)

  override def transpose[B](implicit asIterable: A => Iterable[B]): Nel[LinearSeq[B]] = {
    def fail = throw new IllegalArgumentException("transpose requires all collections have the same size")

    val headSize = asIterable(head).size

    val bs: scala.collection.immutable.IndexedSeq[mutable.Builder[B, LinearSeq[B]]] =
      scala.collection.immutable.IndexedSeq.fill(headSize)(iterableFactory.newBuilder[B])

    for (xs <- iterator) {
      var i = 0
      for (x <- asIterable(xs)) {
        if (i >= headSize) fail
        bs(i) += x
        i += 1
      }
      if (i != headSize)
        fail
    }
    Nel.from(bs.map(_.result())).get
  }

  override def grouped(size: Int): Iterator[Nel[A]] = iterator.grouped(size).map(Nel.from(_).get)
  override def sliding(size: Int) = iterator.sliding(size).map(Nel.from(_).get)
  override def sliding(size: Int, step: Int) = iterator.sliding(size, step).map(Nel.from(_).get)
  override def groupBy[K](f: A => K): Map[K, Nel[A]] = {
    val m = mutable.Map.empty[K, mutable.Builder[A, Nel[A]]]
    val it = iterator
    while (it.hasNext) {
      val elem = it.next()
      val key = f(elem)
      val bldr = m.getOrElseUpdate(key, Nel.newBuilder(elem))
    }
    var result = collection.immutable.HashMap.empty[K, Nel[A]]
    val mapIt = m.iterator
    while (mapIt.hasNext) {
      val (k, v) = mapIt.next()
      result = result.updated(k, v.result())
    }
    result
  }

  override def groupMap[K, B](key: A => K)(f: A => B): Map[K, Nel[B]] = {
    val m = mutable.Map.empty[K, mutable.Builder[B, Nel[B]]]
    for (elem <- this) {
      val k = key(elem)
      val bldr = m.getOrElseUpdate(k, Nel.newBuilder[B](f(elem)))
    }
    val result = Map.newBuilder[K, Nel[B]]
    m.foreach { case (k, v) =>
      result.addOne((k, v.result()))
    }
    result.result()
  }

  override def scanRight[B](z: B)(op: (A, B) => B): Nel[B]  = {
    var scanned = Nel(z)
    var acc = z
    val reversedIter = reversed.iterator
    while (reversedIter.hasNext) {
      acc = op(reversedIter.next(), acc)
      scanned = scanned.prepended(acc)
    }
    scanned
  }

  def flatMap[B](f: A => Nel[B]) = {
    val firstResult = f(head)
    Nel.newBuilder(firstResult.head).addAll(firstResult.tail).addAll(tail.flatMap(f)).result()
  }

  def zip[B](that: Nel[B]): Nel[(A, B)] = new Nel((head, that.head), tail.zip(that.tail))

  override def zipWithIndex: Nel[(A, Int)] = fromList(toList.zipWithIndex)

  override def scanLeft[B](z: B)(op: (B, A) => B) = {
    val b = Nel.newBuilder[B](z)
    b.sizeHint(toIterable, delta = 0)
    var acc = z
    val it = iterator
    while (it.hasNext) {
      acc = op(acc, it.next())
      b += acc
    }
    b.result()
  }

  override def tapEach[U](f: A => U): Nel[A] = {
    foreach(f)
    this
  }

  override def distinct: Nel[A] = distinctBy(identity)

  override def nonEmpty = false
}

object Nel {

  def from[A](source: IterableOnce[A]): Option[Nel[A]] = source match {
    case nel: Nel[A] => Some(nel)
    case h :: t => Some(new Nel(h, t))
    case other =>
      val iter = other.iterator
      if (iter.hasNext) {
        val head = iter.next()
        Some(new Nel(head, List.from(iter)))
      } else None
  }

  def newBuilder[A]: mutable.Builder[A, Option[Nel[A]]] = List.newBuilder[A].mapResult {
    case h :: t => Some(new Nel(h, t))
    case _ => None
  }

  def newBuilder[A](head: A): mutable.Builder[A, Nel[A]] = List.newBuilder[A].mapResult(new Nel(head, _))

  def apply[A](head: A, tail: A*): Nel[A] = new Nel(head, tail.toList)
}
