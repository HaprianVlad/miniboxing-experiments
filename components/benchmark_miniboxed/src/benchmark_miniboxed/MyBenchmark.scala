package benckmark_miniboxed

import scala.reflect.ClassTag
import scala.util.Random._
import com.google.caliper.Runner
import com.google.caliper.SimpleBenchmark
import scala.reflect.ClassTag
import scala.annotation.tailrec


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
      case "sorted" => Sorting.sort(data)(order, ct)
      case "reversed" => Sorting.sort(data)(order.reverse, ct)
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

/*******************************************************************************************/

//MINIBOXED SPIRE IMPLEMENTATION

////////////////////////////////////////////////////////////////////////////////////////////

// Sorting
trait Sort {
  def sort[@miniboxed A:Order:ClassTag](data:Array[A]): Unit
}


object InsertionSort extends Sort {
  final def sort[@miniboxed A:Order:ClassTag](data:Array[A]) =
    sort(data, 0, data.length)

  final def sort[@miniboxed A](data:Array[A], start:Int, end:Int)
    (implicit o:Order[A], ct:ClassTag[A]) {

    var i = start + 1
    while (i < end) {
      val item = data(i)
      var hole = i
      while (hole > start && o.gt(data(hole - 1), item)) {
        data(hole) = data(hole - 1)
        hole -= 1
      }
      data(hole) = item
      i += 1
    }
  }
}


object MergeSort extends Sort {
  @inline final def startWidth = 8
  @inline final def startStep = 16

  final def sort[@miniboxed A:Order:ClassTag](data:Array[A]) {
    val len = data.length

    if (len <= startStep) return InsertionSort.sort(data)

    var buf1:Array[A] = data
    var buf2:Array[A] = new Array[A](len)
    var tmp:Array[A] = null

    var i = 0
    var limit = len - startWidth
    while (i < limit) { InsertionSort.sort(data, i, i + startWidth); i += startWidth }
    if (i < len) InsertionSort.sort(data, i, len)
    var width = startWidth
    var step = startStep
    while (width < len) {
      i = 0
      limit = len - step
      while (i < limit) {
        merge(buf1, buf2, i, i + width, i + step); i += step
      }
      while (i < len) {
        merge(buf1, buf2, i, math.min(i + width, len), len); i += step
      }
      tmp = buf2
      buf2 = buf1
      buf1 = tmp

      width *= 2
      step *= 2
    }

    if (buf1 != data) System.arraycopy(buf1, 0, data, 0, len)
  }


  @inline final def merge[@miniboxed A]
    (in:Array[A], out:Array[A], start:Int, mid:Int, end:Int)
    (implicit o:Order[A]) {

    var ii = start
    var jj = mid
    var kk = start
    while (kk < end) {
      if (ii < mid && (jj >= end || o.lteqv(in(ii), in(jj)))) {
        out(kk) = in(ii); ii += 1
      } else {
        out(kk) = in(jj); jj += 1
      }
      kk += 1
    }
  }
}


object QuickSort {
  @inline final def limit = 16
  final def sort[@miniboxed A:Order:ClassTag](data:Array[A]) = qsort(data, 0, data.length - 1)
  final def qsort[@miniboxed A]
    (data:Array[A], left: Int, right: Int)
    (implicit o:Order[A], ct:ClassTag[A]) {
    if (right - left < limit) return InsertionSort.sort(data, left, right + 1)
    val pivot = left + (right - left) / 2
    val next = partition(data, left, right, pivot)
    qsort(data, left, next - 1)
    qsort(data, next + 1, right)
  }

  final def partition[@miniboxed A]
    (data:Array[A], left:Int, right:Int, pivot:Int)
    (implicit o:Order[A], ct:ClassTag[A]): Int = {
    val value = data(pivot)
    var tmp = data(pivot); data(pivot) = data(right); data(right) = tmp
    var store = left
    var i = left
    while (i < right) {
      if (o.lt(data(i), value)) {
      
        tmp = data(i); data(i) = data(store); data(store) = tmp
        store += 1
      }
      i += 1
    }
    tmp = data(store); data(store) = data(right); data(right) = tmp
    store
  }
}


object Sorting {
  final def sort[@miniboxed A:Order:ClassTag](data:Array[A]) = QuickSort.sort(data)

  final def insertionSort[@miniboxed A:Order:ClassTag](data:Array[A]) = InsertionSort.sort(data)
  final def mergeSort[@miniboxed A:Order:ClassTag](data:Array[A]) = MergeSort.sort(data)
  final def quickSort[@miniboxed K:Order:ClassTag](data:Array[K]) = QuickSort.sort(data)
}
