package scala.collection.immutable

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import org.openjdk.jmh.runner.IterationType
import benchmark._
import java.util.concurrent.TimeUnit

import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class VectorMapBenchmark {
  @Param(Array("1", "10", "100", "1000", "1000000"))
  var size: Int = _

  var kvs: Iterable[(Int, Int)] = _
  var key: Int = 0
  var vmap: VectorMap[Int, Int] = _

  @Setup(Level.Trial)
  def initKeys(): Unit = {
    val unique = (0 to size).map(i => i -> i)
    kvs = unique ++ unique
    key = Random.nextInt(size)
    vmap = VectorMap.from(kvs)
  }

  @Benchmark
  def builder(bh: Blackhole): Unit = {
    val b = VectorMap.newBuilder[Int, Int]
    bh.consume(b.addAll(kvs).result())
  }

  @Benchmark
  def apply(bh: Blackhole): Unit = {
    bh.consume(vmap(key))
  }
  @Benchmark
  def updated(bh: Blackhole): Unit = {
    bh.consume(vmap.updated(key, key + 1))
  }
  @Benchmark
  def removed(bh: Blackhole): Unit = {
    bh.consume(vmap.removed(key))
  }
  @Benchmark
  def foreach(bh: Blackhole): Unit = {
    vmap.foreach { case (k, v) => bh.consume(k); bh.consume(v)}
  }
}
