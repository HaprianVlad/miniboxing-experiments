package benchmark_miniboxed

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Random
import Random._
import com.google.caliper.Runner
import com.google.caliper.SimpleBenchmark
import com.google.caliper.Param

import macroM._


//ARRAY ORDER BENCHMARK MINIBOXED
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
  
  //Implicit definitions
  implicit object myOrderInt extends Order[Int]{
	def compare(x:Int,y:Int):Int = 
    	  if(x<y) -1
      	  else if (x > y) 1
      	  else 0
  }
  
  implicit object myRingInt extends Ring[Int]{
    def zero = 0
    def one = 1
    def negate(x:Int) = -x
    def plus(x:Int,y:Int) = x+y
    def times(x:Int,n:Int) = x*n
  }
   implicit object arrayMonoid extends Monoid[Array[Int]]{
   def id = new Array(0)
   def op(x:Array[Int],y:Array[Int]) = ArraySupport.plus(x, y)
 }
  

  override protected def setUp() {
    val size = math.pow(2, pow).toInt

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
    while (i < x.length && i < y.length && x(i) == y(i)) i += 1
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

 
  def indirectAdd[@miniboxed A: ClassTag: Ring](x: Array[A], y: Array[A]): Array[A] =
   ArraySupport.plus(x, y)

  def directAdd(x: Array[Int], y: Array[Int]): Array[Int] = {
    val z = new Array[Int](math.max(x.length, y.length))
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

  //def timeAddGeneric(reps: Int) = run(reps) {a+b }
  def timeAddIndirect(reps: Int) = run(reps) { indirectAdd(a, b) }
  def timeAddDirect(reps: Int) = run(reps) { directAdd(a, b) }
}

/********************************************************************************************************************/

// All this for + operation in ArrayOrderBenchmark(timeAddGeneric)

trait VectorSpace[V, @miniboxed F] extends Module[V, F] {
  implicit def scalar: Field[F]

  def divr(v: V, f: F): V = timesl(scalar.reciprocal(f), v)
}

object VectorSpace {
  @inline final def apply[V, @miniboxed R](implicit V: VectorSpace[V, R]) = V
}

trait Module[V, @miniboxed R] extends AdditiveAbGroup[V] {
  implicit def scalar: Rng[R]

  def timesl(r: R, v: V): V
  def timesr(v: V, r: R): V = timesl(r, v)
}

object Module {
  @inline final def apply[V, @miniboxed R](implicit V: Module[V, R]) = V

  implicit def IdentityModule[@miniboxed V](implicit ring: Ring[V]) = {
    new IdentityModule[V] {
      val scalar = ring
    }
  }
}

trait IdentityModule[@miniboxed V] extends Module[V, V] {
  def zero = scalar.zero
  def negate(v: V) = scalar.negate(v)
  def plus(v: V, w: V): V = scalar.plus(v, w)
  override def minus(v: V, w: V): V = scalar.minus(v, w)

  def timesl(r: V, v: V): V = scalar.times(r, v)
}

final case class ZModule[V](vector: Group[V]) extends Module[V, Int] {
  def scalar = new IntAlgebra

  def zero: V = vector.id
  def negate(v: V): V = vector.inverse(v)
  def plus(v: V, w: V): V = vector.op(v, w)
  def timesl(k: Int, v: V): V = vector.sumn(v, k)
}

 final  class ArrayVectorSpace[@miniboxed A: ClassTag: Field]
    (implicit nnvs: NoImplicit[NormedVectorSpace[Array[A], A]])
    extends VectorSpace[Array[A], A] with Serializable {
  def scalar = Field[A]
  def zero: Array[A] = new Array[A](0)
  def plus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.plus(x, y)
  def negate(x:Array[A]):Array[A] =ArraySupport.negate(x)
  def timesl(r: A, x: Array[A]): Array[A] = ArraySupport.timesl(r, x)
}
final  class ArrayModule[@miniboxed A: ClassTag: Ring]
    (implicit nvs: NoImplicit[VectorSpace[Array[A], A]])
    extends Module[Array[A], A] with Serializable {
  def scalar = Ring[A]
  def zero: Array[A] = new Array[A](0)
 
  def plus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.plus(x, y)
   def negate(x:Array[A]):Array[A] =ArraySupport.negate(x)
   def timesl(r: A, x: Array[A]): Array[A] = ArraySupport.timesl(r, x)
}

final class ArrayEq[@miniboxed A: Eq]
    extends Eq[Array[A]] with Serializable {
  def eqv(x: Array[A], y: Array[A]): Boolean = ArraySupport.eqv(x, y)
}

////////////////////////////////////////////////////////////////////////////////////////////

//NoImplicit
                   
final class NoImplicit[A]

object NoImplicit {
  implicit def noImplicit0[A] = new NoImplicit[A]
  implicit def noImplicit1[A](implicit ev: A) = new NoImplicit[A]
}

///////////////////////////////////////////////////////////////////////////////////////////////

//Metric and NormedVector spaces

trait MetricSpace[V, @miniboxed R] {
  def distance(v: V, w: V): R
}

trait NormedVectorSpace[V, @miniboxed F]
extends VectorSpace[V, F] with MetricSpace[V, F] {
  def norm(v: V): F

  def normalize(v: V): V = divr(v, norm(v))
  def distance(v: V, w: V): F = norm(minus(v, w))
}

///////////////////////////////////////////////////////////////////////////////////////////

//ArrayOrder and ArrayMonoid

final class ArrayOrder[@miniboxed A: Order]
    extends Order[Array[A]] with Serializable {
  override def eqv(x: Array[A], y: Array[A]): Boolean = ArraySupport.eqv(x, y)
  def compare(x: Array[A], y: Array[A]): Int = ArraySupport.compare(x, y)
}


final class ArrayMonoid[@miniboxed A: ClassTag]
    extends Monoid[Array[A]] with Serializable {
  def id = new Array[A](0)
  def op(x: Array[A], y: Array[A]) = ArraySupport.concat(x, y)
}


/////////////////////////////////////////////////////////////////////////////////////////
//ArrayInstances

trait ArrayInstances0 {
  type NI0[A] = NoImplicit[VectorSpace[Array[A], A]]

  implicit def ArrayModule[@miniboxed A: NI0: ClassTag: Ring]: Module[Array[A], A] =
    new ArrayModule[A]
}

trait ArrayInstances1 extends ArrayInstances0 {
  type NI1[A] = NoImplicit[NormedVectorSpace[Array[A], A]]

  implicit def ArrayVectorSpace[@miniboxed A: NI1: ClassTag: Field]: VectorSpace[Array[A], A] =
    new ArrayVectorSpace[A]

  implicit def ArrayEq[@miniboxed A: Eq]: Eq[Array[A]] =
    new ArrayEq[A]
}

trait ArrayInstances2 extends ArrayInstances1 {
  //implicit def ArrayInnerProductSpace[@miniboxed(Float, Double) A: Field: ClassTag]: InnerProductSpace[Array[A], A] =
   // new ArrayInnerProductSpace[A]

  implicit def ArrayOrder[@miniboxed A: Order]: Order[Array[A]] =
    new ArrayOrder[A]
}

//trait ArrayInstances3 extends ArrayInstances2 {
  //implicit def ArrayNormedVectorSpace[@miniboxed(Float, Double) A: Field: NRoot: ClassTag]: NormedVectorSpace[Array[A], A] =
    //ArrayInnerProductSpace[A].normed
//}

trait ArrayInstances extends ArrayInstances2 {
  implicit def ArrayMonoid[@miniboxed A: ClassTag]: Monoid[Array[A]] =
    new ArrayMonoid[A]
}
//////////////////////////////////////////////////////////////////////////////////////////////////////

// Actual addition to the scope

trait AnyInstances extends  IntInstances
    with LongInstances
    with FloatInstances
    with DoubleInstances
    with ArrayInstances
