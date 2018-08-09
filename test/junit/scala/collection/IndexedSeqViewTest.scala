package scala.collection

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.annotation.tailrec
import scala.collection.immutable.Vector
import scala.tools.testing.AssertUtil.assertThrows

@RunWith(classOf[JUnit4])
class IndexedSeqViewTest {
  @Test
  def foo: Unit = {
    assertEquals(Seq(3, 4, 5), Vector(1, 2, 3, 4, 5).view.drop(2).toSeq)
  }
  @Test
  def slice: Unit = {
        assertEquals(Seq(), Vector().view.slice(0,0).toSeq)
        assertEquals(Seq(), Vector().view.slice(2,4).toSeq)
        assertEquals(Seq(), Vector(1,1,1).view.slice(2,2).toSeq)
        assertEquals(Seq(1,1), Vector(1,1,1).view.slice(-2,2).toSeq)
        assertEquals(Seq(3,4), Vector(1,2,3,4,5).view.slice(2,4).toSeq)
  }

  @Test
  def reverse: Unit = {
    assertEquals(Seq(), Vector().view.reverse.toSeq)
    assertEquals(Seq(1), Vector(1).view.reverse.toSeq)
    val a = Vector(2, 1)
    val b = a.view
    val c = b.reverse
    val d = c.toSeq
    assertEquals(Seq(1, 2), Vector(2, 1).view.reverse.toSeq)
    assertEquals(Seq(1, 1), Vector(1, 1, 1).view.slice(-2, 2).toSeq)
    assertEquals(Seq(3, 4), Vector(1, 2, 3, 4, 5).view.slice(2, 4).toSeq)
  }

}

@RunWith(classOf[JUnit4])
class IndexedSeqViewSliceTest {
  // generates pairs of slices with their corresponding equivalent indexedSeq, to cross-check behavior
  // The slices generated vary in, size of prefix, size of underlying collection, and size of suffix
  val MaxPrefixLength = 10
  val MaxUnderlyingLength = 10
  val MaxSuffixLength = 10
  val maxLength = MaxPrefixLength + MaxUnderlyingLength + MaxSuffixLength

  val allSlices: Seq[(IndexedSeqView[Int], IndexedSeq[Int], (Int, Int, Int))] = {
    val goingForward = for {
      prefixLength <- 0 to MaxPrefixLength
      underlyingLength <- 0 to MaxUnderlyingLength
      suffixLength <- 0 to MaxSuffixLength
    } yield {
      val indexedSeq = 0 until (prefixLength + underlyingLength + suffixLength)
      val slice =
        (0 until suffixLength).foldLeft(
          (0 until prefixLength).reverse.foldLeft(
            (prefixLength until (prefixLength + underlyingLength)).view
          )(_ prepended _)
        )((acc, elem) => acc.appended(elem + prefixLength + underlyingLength))

      (slice, indexedSeq.toVector, (prefixLength, underlyingLength, suffixLength))
    }

    val reverse = goingForward.map {
      case (slice, seq, sizes) => (slice.reverse, seq.reverse, sizes)
    }

    goingForward ++ reverse
  }

  @Test
  def foo: Unit = {
    assertEquals(Seq(3, 4, 5), Vector(1, 2, 3, 4, 5).view.drop(2).toSeq)
  }
  @Test
  def slice: Unit = {
    assertEquals(Seq(), Vector().view.slice(0,0).toSeq)
    assertEquals(Seq(), Vector().view.slice(2,4).toSeq)
    assertEquals(Seq(), Vector(1,1,1).view.slice(2,2).toSeq)
    assertEquals(Seq(1,1), Vector(1,1,1).view.slice(-2,2).toSeq)
    assertEquals(Seq(3,4), Vector(1,2,3,4,5).view.slice(2,4).toSeq)
  }

  @Test
  def reverse: Unit = {
    assertEquals(Seq(), Vector().view.reverse.toSeq)
    assertEquals(Seq(1), Vector(1).view.reverse.toSeq)
    val a = Vector(2, 1)
    val b = a.view
    val c = b.reverse
    val d = c.toSeq
    assertEquals(Seq(1, 2), Vector(2, 1).view.reverse.toSeq)
    assertEquals(Seq(1, 1), Vector(1, 1, 1).view.slice(-2, 2).toSeq)
    assertEquals(Seq(3, 4), Vector(1, 2, 3, 4, 5).view.slice(2, 4).toSeq)
  }


  @Test
  def stackSafeTail: Unit = {
    val coll = Vector.fill(12000)(0)

    def makeStackedSlice(coll: IndexedSeq[Int], count: Int) = {
      @tailrec
      def makeSlicesOfView(view: View[Int], noOfSlices: Int): View[Int] =
        if (noOfSlices == 0) view
        else makeSlicesOfView(view.tail, noOfSlices - 1)

      makeSlicesOfView(coll.view, count)
    }

    val slice = makeStackedSlice(coll, 10000)
    assertEquals(slice.head, 0)
  }

  @Test
  def stackSafe2: Unit = {

    val NumStages = 12
    val NumIterations = 100 * 1000 // hundred thousand

    val seq = (0 until (NumIterations * NumStages)).view
      .map(_ % NumStages)
      .foldLeft((1 to 10).view) {
        case (acc, 0) => acc.appended(-1)
        case (acc, 1) => acc.take(10)
        case (acc, 2) => acc.appended(-2)
        case (acc, 3) => acc.dropRight(1)
        case (acc, 4) => acc.reverse
        case (acc, 5) => acc.reverse

        case (acc, 6) => acc.slice(1, 9)
        case (acc, 7) => acc.prepended(1).appended(10)

        case (acc, 8) => acc.drop(5)
        case (acc, 9) =>
          acc.prepended(5).prepended(4).prepended(3).prepended(2).prepended(1)

        case (acc, 10) => acc.takeRight(9).map(identity)
        case (acc, 11) => acc.prepended(1)
        case (acc, _)  => acc
      }
      .toSeq

    assertEquals(1 to 10, seq)
  }

  @Test
  def sliceExchaustive: Unit = {
    for {
      (slice, indexedSeq, sizes) <- allSlices
      from <- -3 to maxLength + 3
      until <- -3 to maxLength + 2
      fromRange = indexedSeq.slice(from, until).toList
      fromView = slice.slice(from, until).toSeq
    } assert(fromRange == fromView,
      s"""Failed on slice=${slice.toSeq}(sizes=$sizes),indexedSeq=$indexedSeq,from=$from,until=$until
         |range: $fromRange
         |view: $fromView
     """.stripMargin)
  }

  @Test
  def reverseExhaustive: Unit = {
    for {
      (slice, indexedSeq, _) <- allSlices
      fromRange = indexedSeq.reverse.toSeq
      fromView = slice.reverse.toSeq
    } {
      assert(fromRange == fromView,
        s"""Failed on slice=${slice.toSeq},indexedSeq=$indexedSeq
           |range: $fromRange
           |view: $fromView
     """.stripMargin)
      assert(slice.toSeq == slice.reverse.reverse.toSeq,
        s"""Failed on slice=${slice.toSeq},indexedSeq=$indexedSeq
           |range: $fromRange
           |view: $fromView
     """.stripMargin)
    }
  }

  @Test
  def takeExhaustive: Unit = {
    for {
      (slice, indexedSeq, _) <- allSlices
      n <- -3 to maxLength + 3
      fromRange = indexedSeq.take(n).toSeq
      fromView = slice.take(n).toSeq
    } assert(fromRange == fromView,
      s"""Failed on slice=${slice.toSeq},indexedSeq=$indexedSeq,n=$n
         |range: $fromRange
         |view: $fromView
     """.stripMargin)
  }

  @Test
  def takeRightExhaustive: Unit = {
    for {
      (slice, indexedSeq, sizes) <- allSlices
      n <- -3 to maxLength + 3
      fromRange = indexedSeq.takeRight(n).toVector
      fromView = slice.takeRight(n).toSeq
    } assert(fromRange == fromView,
      s"""Failed on slice=${slice.toSeq}(sizes=$sizes),indexedSeq=$indexedSeq,n=$n
         |range: $fromRange
         |view: $fromView
     """.stripMargin)
  }

  @Test
  def dropExhaustive: Unit = {
    for {
      (slice, indexedSeq, sizes) <- allSlices
//          .filter(_._1.toSeq == List(2,1,0))
//          .filter(_._3 == (0,1,2))
//          .take(1)
      n <- -3 to maxLength + 3
//      if n == 1
      fromRange = indexedSeq.drop(n).toList
      fromView = slice.drop(n).toSeq
    } assert(fromRange == fromView,
      s"""Failed on slice=${slice.toSeq}(sizes=$sizes),indexedSeq=$indexedSeq,n=$n
         |range: $fromRange
         |view: $fromView
     """.stripMargin)
  }

  @Test
  def dropRightExhaustive: Unit = {
    for {
      (slice, indexedSeq, sizes) <- allSlices
      n <- -3 to maxLength + 3
      fromRange = indexedSeq.dropRight(n).toList
      fromView = slice.dropRight(n).toList
    } assert(fromRange == fromView,
      s"""Failed on slice=${slice.toSeq}(sizes=$sizes),indexedSeq=$indexedSeq,n=$n
         |range: $fromRange
         |view: $fromView
     """.stripMargin)
  }

  @Test
  def mapExhaustive: Unit = {
    for {
      (slice, indexedSeq, _) <-
        // this test is a bit meaty, we can just sample 5%
        allSlices
          .zipWithIndex
          .collect { case (elem, i) if i % 10 == 0 => elem}
      depthOfMaps <- List(1, 10, 20000)
      fromRange = indexedSeq.map(_ + depthOfMaps).toList
      fromView = (1 to depthOfMaps).foldLeft(slice)((acc, _) => acc.map(_ + 1)).toSeq
    } assert(fromRange == fromView,
      s"""Failed on slice=${slice.toSeq},indexedSeq=$indexedSeq,depthOfMaps=$depthOfMaps
         |range: $fromRange
         |view: $fromView
     """.stripMargin)
  }

  @Test
  def prependExhaustive: Unit = {
    for {
      (slice, indexedSeq, _) <- allSlices
      depthOfMaps <- List(1, 10, 20000)
      fromSeq = indexedSeq.prepended(-100).toList
      fromView = slice.prepended(-100).toList
    } assert(fromSeq == fromView,
      s"""Failed on slice=${slice.toSeq},indexedSeq=$indexedSeq,depthOfMaps=$depthOfMaps
         |range: $fromSeq
         |view: $fromView
     """.stripMargin)
  }

  @Test
  def appendExhaustive: Unit = {
    for {
      (slice, indexedSeq, _) <- allSlices
      depthOfMaps <- List(1, 10, 20000)
      fromSeq = indexedSeq.appended(-100).toList
      fromView = slice.appended(-100).toList
    } assert(fromSeq == fromView,
      s"""Failed on slice=${slice.toSeq},indexedSeq=$indexedSeq,depthOfMaps=$depthOfMaps
         |range: $fromSeq
         |view: $fromView
     """.stripMargin)
  }

  @Test
  def stackSafeAppend: Unit = {
    assertEquals(100001,
      (1 to 100000).foldLeft(Vector(0).view)(_.appended(_)).length)
  }

  @Test
  def stackSafePrepend: Unit = {
    assertEquals(100001,
      (1 to 100000).foldLeft(Vector(0).view)(_.prepended(_)).length)
  }

  @Test
  def mapMultipleTypesTest: Unit = {
    assertEquals(List(10),
      Vector(1).view
        .map(_.toString)
        .map(_.headOption)
        .map(_.map(_.toString.toInt))
        .map(_.map(_ * 10))
        .map(_.getOrElse(5))
        .toSeq)
  }

  @Test
  def mapIsLazyUntilForced: Unit = {
    for ((slice, _, _) <- allSlices) {
      var canary = 0
      val mapped = slice.map { _ => canary += 1; 1 }
      assertEquals(0, canary)

      val forced = mapped.toSeq
      assertEquals(canary > 0, slice.nonEmpty)
    }
  }
}

@RunWith(classOf[JUnit4])
class EmptyIndexedSeqViewTest {
  val empty = IndexedSeqView.Empty
  @Test
  def apply: Unit = {
    assertThrows[IndexOutOfBoundsException](empty(-1))
    assertThrows[IndexOutOfBoundsException](empty(0))
    assertThrows[IndexOutOfBoundsException](empty(1))
  }
  @Test
  def length: Unit = {
    assertEquals(0, empty.length)
    assert(empty.isEmpty)
    assert(!empty.nonEmpty)
  }
  @Test
  def prepended: Unit = {
    assertEquals(List(1), empty.prepended(1).toList)
    assertEquals(List(1,2), empty.prepended(2).prepended(1).toList)
  }
  @Test
  def take: Unit = {
    assertEquals(empty.take(-1), empty)
    assertEquals(empty.take(0), empty)
    assertEquals(empty.take(1), empty)
    assertEquals(empty.take(100), empty)
  }
  @Test
  def takeRight: Unit = {
    assertEquals(empty.takeRight(-1), empty)
    assertEquals(empty.takeRight(0), empty)
    assertEquals(empty.takeRight(1), empty)
    assertEquals(empty.takeRight(100), empty)
  }
  @Test
  def drop: Unit = {
    assertEquals(empty.drop(-1), empty)
    assertEquals(empty.drop(0), empty)
    assertEquals(empty.drop(1), empty)
    assertEquals(empty.drop(100), empty)
  }
  @Test
  def dropRight: Unit = {
    assertEquals(empty.dropRight(-1), empty)
    assertEquals(empty.dropRight(0), empty)
    assertEquals(empty.dropRight(1), empty)
    assertEquals(empty.dropRight(100), empty)
  }
  @Test
  def map: Unit = {
    assertEquals(empty.map(_ => 1), empty)

    var canary = 0
    val mapped = empty.map { _ => canary += 1; 1 }
    assertEquals(0, canary)
    assertEquals(Nil, mapped.toList)
    assertEquals(0, canary)
  }
  @Test
  def reverse: Unit = {
    assertEquals(empty.reverse, empty)
  }
  @Test
  def slice: Unit = {
    assertEquals(empty.slice(-1, -1), empty)
    assertEquals(empty.slice(-1, 0), empty)
    assertEquals(empty.slice(-1, 3), empty)
    assertEquals(empty.slice(0, 3), empty)
    assertEquals(empty.slice(3, 3), empty)
  }
  @Test
  def appended: Unit = {
    assertEquals(empty.appended(1).toList,List(1))
    assertEquals(empty.appended(1).appended(2).toList,List(1, 2))
  }
}

@RunWith(classOf[JUnit4])
class SingleIndexedSeqViewTest {
  val single = new IndexedSeqView.Single(1)
  val empty = IndexedSeqView.Empty
  @Test
  def apply: Unit = {
    assertThrows[IndexOutOfBoundsException](single(-1))
    assertEquals(1, single(0))
    assertThrows[IndexOutOfBoundsException](single(1))
    assertThrows[IndexOutOfBoundsException](single(100))
  }
  @Test
  def length: Unit = {
    assertEquals(1, single.length)
    assert(!single.isEmpty)
    assert(single.nonEmpty)
  }
  @Test
  def prepended: Unit = {
    assertEquals(List(1,1), single.prepended(1).toList)
    assertEquals(List(1,2,1), single.prepended(2).prepended(1).toList)
  }
  @Test
  def take: Unit = {
    assertEquals(single.take(-1), empty)
    assertEquals(single.take(0), empty)
    assertEquals(single.take(1), single)
    assertEquals(single.take(100), single)
  }
  @Test
  def takeRight: Unit = {
    assertEquals(single.takeRight(-1), empty)
    assertEquals(single.takeRight(0), empty)
    assertEquals(single.takeRight(1), single)
    assertEquals(single.takeRight(100), single)
  }
  @Test
  def drop: Unit = {
    assertEquals(single.drop(-1), single)
    assertEquals(single.drop(0), single)
    assertEquals(single.drop(1), empty)
    assertEquals(single.drop(100), empty)
  }
  @Test
  def dropRight: Unit = {
    assertEquals(single.dropRight(-1), single)
    assertEquals(single.dropRight(0), single)
    assertEquals(single.dropRight(1), empty)
    assertEquals(single.dropRight(100), empty)
  }
  @Test
  def map: Unit = {
    assertEquals(single.map(_ => 2).toList, List(2))

    var canary = 0
    val mapped = single.map { _ => canary += 1; 2 }
    assertEquals(0, canary)
    assertEquals(List(2), mapped.toList)
    assertEquals(1, canary)
  }
  @Test
  def reverse: Unit = {
    assertEquals(single.reverse, single)
  }
  @Test
  def slice: Unit = {
    assertEquals(single.slice(-1, -1), empty)
    assertEquals(single.slice(-1, 0), empty)
    assertEquals(single.slice(-1, 3), single)
    assertEquals(single.slice(0, 3), single)
    assertEquals(single.slice(3, 3), empty)
  }
  @Test
  def appended: Unit = {
    assertEquals(single.appended(2).toList,List(1,2))
    assertEquals(single.appended(2).appended(3).toList,List(1, 2, 3))
  }
}

@RunWith(classOf[JUnit4])
class PreviouslyEvaluatedIndexedSeqViewTest {
  def make[A](elems: A*) = new IndexedSeqView.PreviouslyEvaluated(elems.toVector)

  @Test
  def apply: Unit = {
    assertThrows[IndexOutOfBoundsException](make()(-1))
    assertThrows[IndexOutOfBoundsException](make()(0))
    assertThrows[IndexOutOfBoundsException](make()(1))

    assertThrows[IndexOutOfBoundsException](make(1)(-1))
    assertEquals(1, make(1)(0))
    assertEquals(2, make(1, 2)(1))

  }
  @Test
  def length: Unit = {
    assertEquals(0, make().length)
    assertEquals(4, make(1,1,1,1).length)
  }
  @Test
  def prepended: Unit = {
    assertEquals(List(1,1,2,3), make(1,2,3).prepended(1).toList)
    assertEquals(List(1), make().prepended(1).toList)
  }

  @Test
  def take: Unit = {
    assert(make().take(-1).isEmpty)
    assert(make().take(0).isEmpty)

    assert(make(1).take(-1).isEmpty)
    assert(make(1,2).take(0).isEmpty)

    assertEquals(make(1).take(1).toList, List(1))
    assertEquals(make(1,2).take(1).toList, List(1))
    assertEquals(make(1,2).take(2).toList, List(1,2))
  }
  @Test
  def takeRight: Unit = {
    assertEquals(make().takeRight(1).toList, Nil)
    assertEquals(make(1,2,3,4,5).takeRight(-1).toList, Nil)
    assertEquals(make(1,2,3,4,5).takeRight(0).toList, Nil)
    assertEquals(make(1,2,3,4,5).takeRight(1).toList, List(5))
    assertEquals(make(1,2,3,4,5).takeRight(3).toList, List(3,4,5))
  }
  @Test
  def drop: Unit = {
    assertEquals(make().drop(1).toList, Nil)
    assertEquals(make(1,2,3,4,5).drop(-1).toList, List(1,2,3,4,5))
    assertEquals(make(1,2,3,4,5).drop(0).toList, List(1,2,3,4,5))
    assertEquals(make(1,2,3,4,5).drop(1).toList, List(2,3,4,5))
    assertEquals(make(1,2,3,4,5).drop(7).toList, Nil)

  }
  @Test
  def dropRight: Unit = {
    assertEquals(make().dropRight(1).toList, Nil)
    assertEquals(make(1,2,3,4,5).dropRight(-1).toList, List(1,2,3,4,5))
    assertEquals(make(1,2,3,4,5).dropRight(0).toList, List(1,2,3,4,5))
    assertEquals(make(1,2,3,4,5).dropRight(1).toList, List(1,2,3,4))
    assertEquals(make(1,2,3,4,5).dropRight(7).toList, Nil)
  }
  @Test
  def map: Unit = {
    assertEquals(make(1,2,3,4,5).map(_ => 2).toList, List(2,2,2,2,2))

    var canary = 0
    val mapped = make(1,2,3,4,5).map { _ => canary += 1; 2 }
    assertEquals(0, canary)
    assertEquals(List(2,2,2,2,2), mapped.toList)
    assertEquals(5, canary)
  }

  @Test
  def reverse: Unit = {
    assertEquals(make().reverse.toSeq, Nil)
    assertEquals(make(1).reverse.toSeq, List(1))
    assertEquals(make(1,2,3).reverse.toSeq, List(3,2,1))
  }
  @Test
  def slice: Unit = {
    assertEquals(make(1).slice(-1, -1).toList, Nil)
    assertEquals(make(1).slice(-1, 0).toList, Nil)
    assertEquals(make(1).slice(-1, 3).toList, List(1))
    assertEquals(make(1).slice(0, 3).toList, List(1))
    assertEquals(make(1).slice(3, 3).toList, Nil)

    assertEquals(make(1,2,3,4,5).slice(1, 3).toList, List(2,3))
  }
  @Test
  def appended: Unit = {
    assertEquals(make().appended(2).toList,List(2))
    assertEquals(make(1).appended(2).toList,List(1,2))
    assertEquals(make(1,2,3,2,1).appended(2).toList,List(1,2,3,2,1,2))
  }
}
