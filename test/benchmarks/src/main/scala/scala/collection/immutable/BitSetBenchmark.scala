package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 7)
@Measurement(iterations = 7)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class BitSetBenchmark {

  @Param(Array("0", "1", "10", "1000", "10000", "1000000"))
  var size: Int = _

  var bitSet: BitSet = _
  var arg: BitSet = _

  @Setup(Level.Iteration) def initNumbers: Unit = {
    arg = (1 to size / 3).map(_ => Random.nextInt(size)).to(BitSet)
    bitSet = (1 to size / 3).map(_ => Random.nextInt(size)).to(BitSet)
  }

  @Benchmark
  def newRemoveAll(bh: Blackhole): Unit = {
    (1 to 10) foreach { _ =>
      bh.consume(bitSet.removeAll(arg))
    }
  }
  @Benchmark
  def oldRemoveAll(bh: Blackhole): Unit = {
    (1 to 10) foreach { _ =>
    }
  }

}
