package benchmark_generic


import scala.reflect.ClassTag

import scala.util.Random._

import com.google.caliper.Runner
import com.google.caliper.SimpleBenchmark
import com.google.caliper.Param

// REX BENCHMARK GENERIC

object RexBenchmarks extends MyRunner(classOf[RexBenchmarks])

class RexBenchmarks extends MyBenchmark with BenchmarkData {
  @Param(Array("10", "12", "14", "16", "18"))
  var pow: Int = 0
  
  implicit object myOrderDouble extends Order[Double]{
	def compare(x:Double,y:Double):Int = 
    	  if(x<y) -1 
      	  else if (x > y) 1
      	  else 0
  }
  implicit object myOrderFloat extends Order[Float]{
	def compare(x:Float,y:Float):Int = 
    	 if(x<y) -1 
    	 else if (x > y) 1
    	 else 0
 }
 

  var fs: Array[Float] = null
  var ds: Array[Double] = null

  override protected def setUp() {
    val size = math.pow(2, pow).toInt
    fs = mkarray(size, "random")(nextGaussian.toFloat)
    ds = mkarray(size, "random")(nextGaussian)
  }

  def timeDirect(reps:Int): Unit = run(reps)(runDirect(fs, ds, 20))
  def timeGeneric(reps:Int): Unit = run(reps)(runGeneric(fs, ds, 20))

  def runDirect(a: Array[Float], b: Array[Double], n: Int): Double = {
    (for (i <- 2 to n by 2) yield nearlyMaxF(a, n) + nearlyMaxD(b, n)).sum
  }

  def runGeneric(a: Array[Float], b: Array[Double], n: Int): Double = {
    (for (i <- 2 to n by 2) yield nearlyMaxG(a, n) + nearlyMaxG(b, n)).sum
  }

  def nearlyMaxF(a: Array[Float], k: Int, start: Int = 0, end: Int = -1): Float = {
    val i0 = if (start >= 0) start else a.length + start
    val i1 = if (end >= 0) end else a.length + end + 1
    val ai = new Array[Float](math.max(k, 0) + 1)
    var i = i0 + 1
    var j = 0
    ai(0) = a(i0)
    while (i < i1) {
      if (a(i) > ai(j)) {
        var h = j - 1
        if (j < k) { ai(j + 1) = ai(j); j += 1 }
        while (h >= 0 && a(i) > ai(h)) { ai(h + 1) = ai(h); h -= 1 }
        ai(h + 1) = a(i)
      } else if (j < k) {
        j += 1
        ai(j) = a(i)
      }
      i += 1
    }
    ai(k)
  }

  def nearlyMaxD(a: Array[Double], k: Int, start: Int = 0, end: Int = -1): Double = {
    val i0 = if (start >= 0) start else a.length + start
    val i1 = if (end >= 0) end else a.length + end + 1
    val ai = new Array[Double](math.max(k, 0) + 1)
    var i = i0 + 1
    var j = 0
    ai(0) = a(i0)
    while (i < i1) {
      if (a(i) > ai(j)) {
        var h = j - 1
        if (j < k) { ai(j + 1) = ai(j); j += 1 }
        while (h >= 0 && a(i) > ai(h)) { ai(h + 1) = ai(h); h -= 1 }
        ai(h + 1) = a(i)
      } else if (j < k) {
        j += 1
        ai(j) = a(i)
      }
      i += 1
    }
    ai(k)
  }

  def nearlyMaxG[A : Numeric: ClassTag](a: Array[A], k: Int, start: Int = 0, end: Int = -1): A = {
    
    val i0 = if (start >= 0) start else a.length + start
    val i1 = if (end >= 0) end else a.length + end + 1
    val ai = new Array[A](math.max(k, 0) + 1)
    var i = i0 + 1
    var j = 0
    ai(0) = a(i0)
    while (i < i1) {
      if (implicitly[Numeric[A]].gt(a(i),ai(j))) {
        var h = j - 1
        if (j < k) { ai(j + 1) = ai(j); j += 1 }
        while (h >= 0 && implicitly[Numeric[A]].gt(a(i),ai(h))) { ai(h + 1) = ai(h); h -= 1 }
        ai(h + 1) = a(i)
      } else if (j < k) {
        j += 1
        ai(j) = a(i)
      }
      i += 1
    }
    ai(k)
   
  }
  
  
}
