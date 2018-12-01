package scala.collection.mutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class BitSetBenchmarks {

  @Param(
    Array(
      "0",
      "100",
      /*"200",
      "300",
      "400",
      "500",
      "600",
      "700",
      "800",
      "900",*/

//      "1000",
//      "10000",
//      "100000",
//      "1000000",

//      "10000000",
//      "100000000",
    ))
  var size: Int = _

  var receiverBitSet: BitSet = _

  def makeNewBitset: BitSet = BitSet.fromBitMask(receiverBitSet.toBitMask)

  /** Should contain elements from the same range as receiver */
  var argBitset1: BitSet = _

  /** Should contain elements from half the range as receiver */
  var argBitset2: BitSet = _

  /** Should contain elements from thrice the range as receiver */
  var argBitset3: BitSet = _

  @Setup(Level.Iteration)
  def setValues(): Unit = {
    def bitSetForSize(n: Int) =
      (0 until size).filter(_ => Random.nextBoolean()).to(BitSet)
    receiverBitSet = bitSetForSize(size)
    argBitset1 = bitSetForSize(size / 2)
    argBitset2 = bitSetForSize(size)
    argBitset3 = bitSetForSize(size * 3)
  }

  @Benchmark
  def andNot(blackhole: Blackhole): Unit = {
    (1 to 100) foreach { _ =>
      blackhole.consume(makeNewBitset &~= argBitset1)
      blackhole.consume(makeNewBitset &~= argBitset2)
      blackhole.consume(makeNewBitset &~= argBitset3)
    }
  }
  @Benchmark
  def oldAndNot(blackhole: Blackhole): Unit = {
    (1 to 100) foreach { _ =>
      blackhole.consume(makeNewBitset old_&~= argBitset1)
      blackhole.consume(makeNewBitset old_&~= argBitset2)
      blackhole.consume(makeNewBitset old_&~= argBitset3)
    }
  }
}
