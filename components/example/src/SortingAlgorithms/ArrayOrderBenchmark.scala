package SortingAlgorithms

import scala.{specialized => spec}
import scala.annotation.tailrec
import scala.reflect.ClassTag

import scala.util.Random
import Random._

import spire.algebra._
import spire.implicits._

import com.google.caliper.Runner 
import com.google.caliper.SimpleBenchmark
import com.google.caliper.Param

object ArrayOrderBenchmarks extends MyRunner(classOf[ArrayOrderBenchmarks])

class ArrayOrderBenchmarks extends MyBenchmark {
  @Param(Array("6", "10", "14", "18"))
  var pow: Int = 0

  var a: Array[Int] = null
  var b: Array[Int] = null
  var c: Array[Int] = null
  var d: Array[Int] = null
  var e: Array[Int] = null
  var f: Array[Int] = null

  override protected def setUp() {
    val size = spire.math.pow(2, pow).toInt

    a = init(size)(nextInt)
    b = a.clone
    c = a.clone; c(1) += 1
    d = a.clone; d(size / 3) += 1
    e = a.clone; e(size - 7) += 1
    f = init(size + 10)(nextInt); System.arraycopy(a, 0, f, 0, a.length)
  }

  def directEq(x: Array[Int], y: Array[Int]): Boolean = {
    var i = 0
    if (x.length != y.length) return false
    while (i < x.length && i < y.length && x(i) === y(i)) i += 1
    i == x.length
  }

  def directCompare(x: Array[Int], y: Array[Int]): Int = {
    var i = 0
    val ev = Order[Int]
    while (i < x.length && i < y.length) {
      val cmp = ev.compare(x(i), y(i))
      if (cmp != 0) return cmp
      i += 1
    }
    x.length - y.length
  }

  def indirectAdd[@spec(Int) A: ClassTag: Ring](x: Array[A], y: Array[A]): Array[A] =
    spire.std.ArraySupport.plus(x, y)

  def directAdd(x: Array[Int], y: Array[Int]): Array[Int] = {
    val z = new Array[Int](spire.math.max(x.length, y.length))
    var i = 0
    while (i < x.length && i < y.length) { z(i) = x(i) + y(i); i += 1 }
    while (i < x.length) { z(i) = x(i); i += 1 }
    while (i < y.length) { z(i) = y(i); i += 1 }
    z
  }

  // def timeEqGeneric(reps: Int) = run(reps) { a === b }
  // def timeEqDirect(reps: Int) = run(reps) { directEq(a, b) }

  // def timeCompareGeneric(reps: Int) = run(reps) { a compare b }
  // def timeCompareDirect(reps: Int) = run(reps) { directCompare(a, b) }

  def timeAddGeneric(reps: Int) = run(reps) { a + b }
  def timeAddIndirect(reps: Int) = run(reps) { indirectAdd(a, b) }
  def timeAddDirect(reps: Int) = run(reps) { directAdd(a, b) }
}




import scala.reflect.ClassTag

import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.util.Random
import Random._

import spire.algebra._
import spire.math._
import spire.math.algebraic._
import spire.implicits._

import com.google.caliper.Runner
import com.google.caliper.SimpleBenchmark
import com.google.caliper.Param

import java.lang.Math
import java.math.BigInteger

/**
* Extend this to create an actual benchmarking class.
*/
trait MyBenchmark extends SimpleBenchmark {

  /**
* Sugar for building arrays using a per-cell init function.
*/
  def init[A:ClassTag](size:Int)(f: => A) = {
    val data = Array.ofDim[A](size)
    for (i <- 0 until size) data(i) = f
    data
  }

  /**
* Sugar for building arrays using a per-cell init function.
*/
  def mkarray[A:ClassTag:Order](size:Int, layout:String)(f: => A): Array[A] = {
    val data = init(size)(f)
    val ct = implicitly[ClassTag[A]]
    val order = Order[A]
    layout match {
      case "random" =>
      case "sorted" => spire.math.Sorting.sort(data)(order, ct)
      case "reversed" => spire.math.Sorting.sort(data)(order.reverse, ct)
      case _ => sys.error(s"unknown layout: $layout")
    }
    data
  }

  def nextComplex = Complex(nextDouble, nextDouble)

  /**
* Sugar to run 'f' for 'reps' number of times.
*/
  def run[A](reps:Int)(f: => A): A = {
    def loop(a: A, i: Int): A = if (i < reps) loop(f, i + 1) else a
    if (reps < 1) sys.error("!") else loop(f, 1)
  }
}

/**
* Extend this to create a main object which will run 'cls' (a benchmark).
*/
abstract class MyRunner(val cls:java.lang.Class[_ <: com.google.caliper.Benchmark]) {
  def main(args:Array[String]): Unit = Runner.main(cls, args:_*)
}

trait BenchmarkData extends MyBenchmark {
  //val size = 10 * 1000
  //val size = 100 * 1000
  val size = 200 * 1000
  //val size = 1 * 1000 * 1000
  //val size = 4 * 1000 * 1000
  //val size = 20 * 1000 * 1000

  lazy val ints = init(size)(nextInt)
  lazy val longs = init(size)(nextLong)
  lazy val floats = init(size)(nextFloat)
  lazy val doubles = init(size)(nextDouble)
  //lazy val maybeDoubles = init(size)(MaybeDouble(nextDouble))

  lazy val complexes = init(size)(nextComplex)
  lazy val fcomplexes = init(size)(FastComplex(nextFloat(), nextFloat()))
}