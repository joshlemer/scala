package scala.collection.mutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable
import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ArrayIteratorBenchmarks {

//  @Param(Array(
//    "0",
//    "100",
//    /*"200",
//    "300",
//    "400",
//    "500",
//    "600",
//    "700",
//    "800",
//    "900",*/
//    "1000",
//    "10000",
//    "100000",
//    "1000000",
//    "10000000",
//    "100000000",
//  ))
//  var valueCount: Int = _
//
//  var values: Array[Int] = _
//
//  @Setup
//  def setValues(): Unit = {
//    val random: util.Random = new util.Random(0)
//    values = Array.fill(valueCount)(random.nextInt())
//  }
//
//  @Benchmark
//  def arrayIterator(blackhole: Blackhole): Unit = {
//    val i = values.iterator
//    while (i.hasNext) blackhole.consume(i.next())
//  }

  var arrayBuffer: Set[Int] = _

  var arg: Array[Int] = _

  var arrayBuffer1: Set[Int] = _

  var arg1: Array[Int] = _


  @Param(Array(/* "1", "100", */"1000"))
  var size: Int = _


  @Setup(Level.Invocation)
  def setup(): Unit = {
    arrayBuffer = (1 to size).map(_ => Random.nextInt()).to(mutable.Set)
    arg = arrayBuffer.toArray ++ arrayBuffer.toArray

    arrayBuffer1 = (1 to size).map(_ => Random.nextInt()).to(mutable.Set)
    arg1 = (1 to size).map(_ => Random.nextInt()).to(Array)
  }

  @Benchmark
  def subtractAll(bh: Blackhole): Unit = {
//    bh.consume(arrayBuffer.subtractAll(arg))
    bh.consume(arrayBuffer1.subtractAll(arg1))
  }
  @Benchmark
  def oldSubtractAll(bh: Blackhole): Unit = {
//    bh.consume(arrayBuffer1.oldSubtractAll(arg1))
    bh.consume(arrayBuffer1.oldSubtractAll(arg1))
  }
}
