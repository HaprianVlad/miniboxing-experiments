package benchmark

import scala.util.Random
import Random._

import com.google.caliper.{ Runner, SimpleBenchmark, Param }
import org.apache.commons.math3.analysis.polynomials._
import org.apache.commons.math3.analysis.UnivariateFunction

import scala.collection.mutable.ArrayBuilder

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.{specialized => spec}
import java.lang.Long.{ numberOfTrailingZeros, numberOfLeadingZeros }
import scala.math.{ScalaNumber, ScalaNumericConversions}
import java.lang.Math



object PolynomialBenchmarks extends MyRunner(classOf[PolynomialBenchmarks])

class PolynomialBenchmarks extends MyBenchmark {

  @Param(Array("1", "2", "4", "8", "16"))
  var size: Int = 0

  def arbitraryRational = {
    val d = nextLong() % 100
    Rational(nextLong(), if (d == 0L) 1L else d)
  }

  var spireDenseRationalPolys: Array[Polynomial[Rational]] = null
  var spireSparseRationalPolys: Array[Polynomial[Rational]] = null
  var spireDenseDoublePolys: Array[Polynomial[Double]] = null
  var spireSparseDoublePolys: Array[Polynomial[Double]] = null
  var commonsDoublePolys: Array[PolynomialFunction] = null

  override protected def setUp() {

    val coeffs: Array[Array[Rational]] =
      init(100)(init(size)(arbitraryRational))

    spireDenseRationalPolys = coeffs.map(cs => Polynomial.dense(cs))
    spireSparseRationalPolys = spireDenseRationalPolys.map(_.toSparse)
    spireDenseDoublePolys = coeffs.map(cs => Polynomial.dense(cs.map(_.toDouble)))
    spireSparseDoublePolys = spireDenseDoublePolys.map(_.toSparse)
    commonsDoublePolys = coeffs.map(cs => new PolynomialFunction(cs.map(_.toDouble)))
  }

  def addSpireRationalPolynomials(data: Array[Polynomial[Rational]]): Polynomial[Rational] = {
    var total: Polynomial[Rational] = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(0) + data(i); i += 1 }
    total
  }

  def addSpireDoublePolynomials(data: Array[Polynomial[Double]]): Polynomial[Double] = {
    var total: Polynomial[Double] = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(0) + data(i); i += 1 }
    total
  }

  def addCommonsDoublePolynomials(data: Array[PolynomialFunction]): PolynomialFunction = {
    var total: PolynomialFunction = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(0).add(data(i)); i += 1 }
    total
  }

  def multiplySpireRationalPolynomials(data: Array[Polynomial[Rational]]): Polynomial[Rational] = {
    var total: Polynomial[Rational] = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(0) * data(i); i += 1 }
    total
  }

  def multiplySpireDoublePolynomials(data: Array[Polynomial[Double]]): Polynomial[Double] = {
    var total: Polynomial[Double] = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(0) * data(i); i += 1 }
    total
  }

  def multiplyCommonsDoublePolynomials(data: Array[PolynomialFunction]): PolynomialFunction = {
    var total: PolynomialFunction = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(0).multiply(data(i)); i += 1 }
    total
  }

  def derivativeSpireRationalPolynomials(data: Array[Polynomial[Rational]]): Polynomial[Rational] = {
    var total : Polynomial[Rational] = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(i).derivative; i += 1 }
    total
  }

  def derivativeSpireDoublePolynomials(data: Array[Polynomial[Double]]): Polynomial[Double] = {
    var total : Polynomial[Double] = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(i).derivative; i += 1 }
    total
  }

  def derivativeCommonsDoublePolynomials(data: Array[PolynomialFunction]): PolynomialFunction = {
    var total : PolynomialFunction = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(i).polynomialDerivative; i += 1 }
    total
  }

  def evaluateSpireRationalPolynomials(data: Array[Polynomial[Rational]]): Rational = {
    val testVariable = Rational(2, 1)
    var total : Rational = Rational(1,1)
    var i = 0
    val len = data.length
    while (i < len) { total = data(i).apply(testVariable); i += 1 }
    total
  }

  def evaluateSpireDoublePolynomials(data: Array[Polynomial[Double]]): Double = {
    val testVariable = 2.0
    var total : Double = 0.0
    var i = 0
    val len = data.length
    while (i < len) { total = data(i).apply(testVariable); i += 1 }
    total
  }

  def evaluateCommonsDoublePolynomials(data: Array[PolynomialFunction]): Double = {
    val testVariable = 2.0
    var total : Double = 0.0
    var i = 0
    val len = data.length
    while (i < len) { total = data(i).value(testVariable); i += 1 }
    total
  }

  def quotModSpireRationalPolynomials(data: Array[Polynomial[Rational]]): (Polynomial[Rational], Polynomial[Rational]) = {
    var total: (Polynomial[Rational], Polynomial[Rational]) = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(0) /% data(i); i += 1 }
    total
  }

  def quotModSpireDoublePolynomials(data: Array[Polynomial[Double]]): (Polynomial[Double], Polynomial[Double]) = {
    var total: (Polynomial[Double], Polynomial[Double]) = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(0) /% data(i); i += 1 }
    total
  }

  def timeAddSpireRationalPolysSparse(reps: Int) = run(reps)(addSpireRationalPolynomials(spireSparseRationalPolys))
  def timeAddSpireRationalPolysDense(reps: Int) = run(reps)(addSpireRationalPolynomials(spireDenseRationalPolys))
  def timeAddSpireDoublePolysSparse(reps: Int) = run(reps)(addSpireDoublePolynomials(spireSparseDoublePolys))
  def timeAddSpireDoublePolysDense(reps: Int) = run(reps)(addSpireDoublePolynomials(spireDenseDoublePolys))
  def timeAddCommonsDoublePolynomials(reps: Int) = run(reps)(addCommonsDoublePolynomials(commonsDoublePolys))

  def timeMultiplySpireRationalPolysSparse(reps: Int) = run(reps)(multiplySpireRationalPolynomials(spireSparseRationalPolys))
  def timeMultiplySpireRationalPolysDense(reps: Int) = run(reps)(multiplySpireRationalPolynomials(spireDenseRationalPolys))
  def timeMultiplySpireDoublePolysSparse(reps: Int) = run(reps)(multiplySpireDoublePolynomials(spireSparseDoublePolys))
  def timeMultiplySpireDoublePolysDense(reps: Int) = run(reps)(multiplySpireDoublePolynomials(spireDenseDoublePolys))
  def timeMultiplyCommonsDoublePolynomials(reps: Int) = run(reps)(multiplyCommonsDoublePolynomials(commonsDoublePolys))
  
  def timeDerivativeSpireRationalPolysSparse(reps: Int) = run(reps)(derivativeSpireRationalPolynomials(spireSparseRationalPolys))
  def timeDerivativeSpireRationalPolysDense(reps: Int) = run(reps)(derivativeSpireRationalPolynomials(spireDenseRationalPolys))
  def timeDerivativeSpireDoublePolysSparse(reps: Int) = run(reps)(derivativeSpireDoublePolynomials(spireSparseDoublePolys))
  def timeDerivativeSpireDoublePolysDense(reps: Int) = run(reps)(derivativeSpireDoublePolynomials(spireDenseDoublePolys))
  def timeDerivativeCommonsDoublePolynomials(reps: Int) = run(reps)(derivativeCommonsDoublePolynomials(commonsDoublePolys))
  
  def timeEvaluateSpireRationalPolysSparse(reps: Int) = run(reps)(evaluateSpireRationalPolynomials(spireSparseRationalPolys))
  def timeEvaluateSpireRationalPolysDense(reps: Int) = run(reps)(evaluateSpireRationalPolynomials(spireDenseRationalPolys))
  def timeEvaluateSpireDoublePolysSparse(reps: Int) = run(reps)(evaluateSpireDoublePolynomials(spireSparseDoublePolys))
  def timeEvaluateSpireDoublePolysDense(reps: Int) = run(reps)(evaluateSpireDoublePolynomials(spireDenseDoublePolys))
  def timeEvaluateCommonsDoublePolynomials(reps: Int) = run(reps)(evaluateCommonsDoublePolynomials(commonsDoublePolys))

  def timeQuotModSpireRationalPolysSparse(reps: Int) = run(reps)(quotModSpireRationalPolynomials(spireSparseRationalPolys))
  def timeQuotModSpireRationalPolysDense(reps: Int) = run(reps)(quotModSpireRationalPolynomials(spireDenseRationalPolys))
  def timeQuotModSpireDoublePolysSparse(reps: Int) = run(reps)(quotModSpireDoublePolynomials(spireSparseDoublePolys))
  def timeQuotModSpireDoublePolysDense(reps: Int) = run(reps)(quotModSpireDoublePolynomials(spireDenseDoublePolys))

}

/**************************************************************************************************************************/
// PRIVATE SPIRE IMPLEMENTATION
//////////////////////////////////////////////////////////////////////////////////////////////////
//Term
case class Term[@spec(Float, Double) C](coeff: C, exp: Int) { lhs =>

  def unary_-(implicit r: Rng[C]): Term[C] = Term(-coeff, exp)

  def +(rhs: Term[C])(implicit r: Semiring[C]): Term[C] = {
    if (lhs.exp != rhs.exp)
      throw new IllegalArgumentException(s"can't add terms of degree $exp and ${rhs.exp}")
    Term(lhs.coeff + rhs.coeff, lhs.exp)
  }	

  def *(rhs: Term[C])(implicit r: Semiring[C]): Term[C] =
    Term(lhs.coeff * rhs.coeff, lhs.exp + rhs.exp)

  def toTuple: (Int, C) = (exp, coeff)

  def eval(x: C)(implicit r: Semiring[C]): C =
    if (exp != 0) coeff * (x pow exp) else coeff

  def isIndexZero: Boolean =
    exp == 0

  def isZero(implicit ring: Semiring[C], eq: Eq[C]): Boolean =
    coeff === ring.zero

  def divideBy(x: C)(implicit f: Field[C]): Term[C] =
    Term(coeff / x, exp)

  def der(implicit r: Ring[C]): Term[C] =
    Term(coeff * r.fromInt(exp), exp - 1)

  def int(implicit f: Field[C]): Term[C] =
    Term(coeff / f.fromInt(exp + 1), exp + 1)

  override def toString = {
    import Term._

    def expString = exp match {
      case 0 => ""
      case 1 => "x"
      case _ => s"x^$exp"
    }

    def simpleCoeff: Option[String] = coeff match {
      case 0 => Some("")
      case 1 if exp == 0 => Some(s" + $coeff")
      case 1 => Some(s" + $expString")
      case -1 if exp != 0 => Some(s" - $expString")
      case _ => None
    }

    def stringCoeff: Option[String] = coeff.toString match {
      case IsZero() => Some("")
      case IsNegative(posPart) if exp == 0 => Some(s" - $posPart")
      case IsNegative(posPart) => Some(s" - $posPart$expString")
      case _ => None
    }

    simpleCoeff orElse stringCoeff getOrElse s" + $coeff$expString"
  }
}

object Term {
  implicit def ordering[C] = new Order[Term[C]] {
    def compare(x: Term[C], y: Term[C]): Int = x.exp compare y.exp
  }

  def fromTuple[@spec(Float, Double) C](tpl: (Int, C)): Term[C] =
    Term(tpl._2, tpl._1)
  def zero[@spec(Float, Double) C](implicit r: Semiring[C]): Term[C] =
    Term(r.zero, 0)
  def one[@spec(Float, Double) C](implicit r: Rig[C]): Term[C] =
    Term(r.one, 0)

  private val IsZero = "0".r
  private val IsNegative = "-(.*)".r
}
//////////////////////////////////////////////////////////////////////////////////////////////////
//PolySparse

case class PolySparse[@spec(Double) C] private [spire] (val exp: Array[Int], val coeff: Array[C])
    (implicit val ct: ClassTag[C]) extends Polynomial[C] { lhs =>

  def toDense(implicit ring: Semiring[C], eq: Eq[C]): PolyDense[C] =
    Polynomial.dense(coeffsArray)

  def toSparse(implicit ring: Semiring[C], eq: Eq[C]): PolySparse[C] = lhs

  def foreach[U](f: (Int, C) => U): Unit =
    cfor(0)(_ < exp.length, _ + 1) { i => f(exp(i), coeff(i)) }

  override def foreachNonZero[U](f: (Int, C) => U)(implicit ring: Semiring[C], eq: Eq[C]): Unit =
    foreach(f)

  def degree: Int = if (isZero) 0 else exp(exp.length - 1)

  def coeffsArray(implicit ring: Semiring[C]): Array[C] = if (isZero) {
    new Array[C](0)
  } else {
    val cs = new Array[C](degree + 1)
    cfor(0)(_ < cs.length, _ + 1) { i => cs(i) = ring.zero }
    cfor(0)(_ < exp.length, _ + 1) { i =>
      cs(exp(i)) = coeff(i)
    }
    cs
  }

  def nth(n: Int)(implicit ring: Semiring[C]): C = {
    val i = java.util.Arrays.binarySearch(exp, n)
    if (i >= 0) coeff(i) else ring.zero
  }

  def maxOrderTermCoeff(implicit ring: Semiring[C]): C =
    if (isZero) ring.zero else coeff(coeff.length - 1)

  def reductum(implicit e: Eq[C], ring: Semiring[C], ct: ClassTag[C]): Polynomial[C] = {
    var i = coeff.length - 2
    while (i >= 0 && coeff(i) === ring.zero) i -= 1
    if (i < 0) {
      new PolySparse(new Array[Int](0), new Array[C](0))
    } else {
      val len = i + 1
      val es = new Array[Int](len)
      val cs = new Array[C](len)
      System.arraycopy(coeff, 0, cs, 0, len)
      System.arraycopy(exp, 0, es, 0, len)
      new PolySparse(es, cs)
    }
  }

  private final def expBits(x: C)(implicit ring: Semiring[C]): Array[C] = {
    val bits = new Array[C](math.max(2, 32 - numberOfLeadingZeros(degree)))
    bits(0) = x
    // we use pow(2) here for the benefit of Interval[_], where
    // x.pow(2) has better error bounds than than (x * x).
    if (bits.length > 1) bits(1) = x.pow(2)
    cfor(2)(_ < bits.length, _ + 1) { i =>
      val prev = bits(i - 1)
      bits(i) = prev * prev
    }
    bits
  }

  @tailrec
  private final def fastExp(bits: Array[C], e: Int, i: Int, acc: C)(implicit ring: Semiring[C]): C = {
    if (e == 0) acc else {
      val lb = numberOfTrailingZeros(e) + 1
      val j = i + lb
      fastExp(bits, e >>> lb, j, acc * bits(j - 1))
    }
  }

  private final def fastExp(bits: Array[C], e: Int)(implicit ring: Semiring[C]): C = {
    val lb = numberOfTrailingZeros(e) + 1
    fastExp(bits, e >>> lb, lb, bits(lb - 1))
  }

  def isZero: Boolean =
    exp.isEmpty

  def apply(x: C)(implicit ring: Semiring[C]): C = if (isZero) {
    ring.zero
  } else if (exp.length == 1) {
    if (exp(0) != 0) coeff(0) * (x pow exp(0)) else coeff(0)
  } else {
    // TODO: Rewrite this to be more like PolyDense.
    val bits = expBits(x)
    val e0 = exp(0)
    val c0 = coeff(0)
    var sum = if (e0 == 0) c0 else c0 * fastExp(bits, e0)
    cfor(1)(_ < exp.length, _ + 1) { i =>
      sum += coeff(i) * fastExp(bits, exp(i))
    }
    sum
  }

  def derivative(implicit ring: Ring[C], eq: Eq[C]): Polynomial[C] = {
    val i0 = if (exp(0) == 0) 1 else 0
    val es = new Array[Int](exp.length - i0)
    val cs = new Array[C](es.length)

    @tailrec
    def loop(i: Int, j: Int): Unit = if (j < es.length) {
      val e = exp(i)
      es(j) = e - 1
      cs(j) = e * coeff(i)
      loop(i + 1, j + 1)
    }

    loop(i0, 0)
    PolySparse.safe(es, cs)
  }

  def integral(implicit field: Field[C], eq: Eq[C]): Polynomial[C] = {
    val es = new Array[Int](exp.length)
    val cs = new Array[C](es.length)

    cfor(0)(_ < es.length, _ + 1) { i =>
      val e = exp(i) + 1
      es(i) = e
      cs(i) = coeff(i) / field.fromInt(e)
    }
    
    PolySparse.safe(es, cs)
  }

  def unary_-()(implicit ring: Rng[C]): Polynomial[C] = {
    val cs = new Array[C](coeff.length)
    cfor(0)(_ < cs.length, _ + 1) { i => cs(i) = -coeff(i) }
    new PolySparse(exp, cs)
  }

  def +(rhs0: Polynomial[C])(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] = {
    val rhs: PolySparse[C] = PolySparse(rhs0)
    PolySparse.addSparse(lhs, rhs)
  }

  def *(rhs0: Polynomial[C])(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] = {
    val rhs: PolySparse[C] = PolySparse(rhs0)
    PolySparse.multiplySparse(lhs, rhs)
  }

  def /%(rhs: Polynomial[C])(implicit field: Field[C], eq: Eq[C]): (Polynomial[C], Polynomial[C]) = {
    require(!rhs.isZero, "Can't divide by polynomial of zero!")

    PolySparse.quotmodSparse(lhs, PolySparse(rhs))
  }

  def *: (k: C)(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] = {
    if (k === ring.zero) {
      PolySparse.zero[C]
    } else {
      val cs = new Array[C](coeff.length)
      cfor(0)(_ < cs.length, _ + 1) { i =>
        cs(i) = k * coeff(i)
      }
      new PolySparse(exp, cs)
    }
  }
}


object PolySparse {
  private final def dense2sparse[@spec(Double) C: Semiring: Eq: ClassTag](poly: PolyDense[C]): PolySparse[C] = {
    val cs = poly.coeffs
    val es = new Array[Int](cs.length)
    cfor(0)(_ < es.length, _ + 1) { i => es(i) = i }
    PolySparse.safe(es, cs)
  }

  private final def safe[@spec(Double) C: Semiring: Eq: ClassTag]
      (exp: Array[Int], coeff: Array[C]): PolySparse[C] = {
    var len = 0
    cfor(0)(_ < coeff.length, _ + 1) { i =>
      if (coeff(i) =!= Semiring[C].zero)
        len += 1
    }

    if (len == coeff.length) {
      new PolySparse(exp, coeff)
    } else {
      val es = new Array[Int](len)
      val cs = new Array[C](len)
      @tailrec def loop(i: Int, j: Int): PolySparse[C] =
        if (i < coeff.length) {
          val c = coeff(i)
          if (c =!= Semiring[C].zero) {
            es(j) = exp(i)
            cs(j) = c
            loop(i + 1, j + 1)
          } else {
            loop(i + 1, j)
          }
        } else new PolySparse(es, cs)
      loop(0, 0)
    }
  }

  final def apply[@spec(Double) C: Semiring: Eq: ClassTag](data: Map[Int,C]): PolySparse[C] = {
    val data0 = data.toArray
    data0.qsortBy(_._1)
    val es = new Array[Int](data0.length)
    val cs = new Array[C](data0.length)
    cfor(0)(_ < data0.length, _ + 1) { i =>
      val (e, c) = data0(i)
      es(i) = e
      cs(i) = c
    }
    safe(es, cs)
  }

  final def apply[@spec(Double) C: Semiring: Eq: ClassTag](poly: Polynomial[C]): PolySparse[C] = {
    poly match {
      case (poly: PolySparse[_]) =>
        poly

      case (_: PolyDense[_]) =>
        dense2sparse(poly.asInstanceOf[PolyDense[C]]) // Yay...

      case _ =>
        var len = 0
        poly.foreachNonZero { (_, _) => len += 1 }
        val es = new Array[Int](len)
        val cs = new Array[C](len)
        var i = 0
        poly.foreachNonZero { (e, c) =>
          es(i) = e
          cs(i) = c
          i += 1
        }
        PolySparse.safe(es, cs)
    }
  }

  final def zero[@spec(Double) C: Semiring: Eq: ClassTag]: PolySparse[C] =
    new PolySparse(new Array[Int](0), new Array[C](0))

  private final def multiplyTerm[@spec(Double) C: Semiring: Eq: ClassTag](poly: PolySparse[C], c: C, e: Int): PolySparse[C] = {
    val exp = poly.exp
    val coeff = poly.coeff
    val cs = new Array[C](coeff.length)
    val es = new Array[Int](exp.length)
    cfor(0)(_ < coeff.length, _ + 1) { i =>
      cs(i) = c * coeff(i)
      es(i) = exp(i) + e
    }
    new PolySparse(es, cs)
  }

  private final def multiplySparse[@spec(Double) C: Semiring: Eq: ClassTag]
      (lhs: PolySparse[C], rhs: PolySparse[C]): PolySparse[C] = {
    val lexp = lhs.exp
    val lcoeff = lhs.coeff
    var sum = new PolySparse(new Array[Int](0), new Array[C](0))
    cfor(0)(_ < lexp.length, _ + 1) { i =>
      sum = addSparse(sum, multiplyTerm(rhs, lcoeff(i), lexp(i)))
    }
    sum
  }

  private final def countSumTerms[@spec(Double) C]
      (lhs: PolySparse[C], rhs: PolySparse[C], lOffset: Int = 0, rOffset: Int = 0): Int = {
    val PolySparse(lexp, lcoeff) = lhs
    val PolySparse(rexp, rcoeff) = rhs

    @tailrec
    def loop(i: Int, j: Int, count: Int): Int =
      if (i < lexp.length && j < rexp.length) {
        val cmp = lexp(i) + lOffset - rexp(j) - rOffset
        if (cmp == 0) loop(i + 1, j + 1, count + 1)
        else if (cmp < 0) loop(i + 1, j, count + 1)
        else loop(i, j + 1, count + 1)
      } else {
        count + (lexp.length - i) + (rexp.length - j)
      }

    loop(0, 0, 0)
  }

  private final def addSparse[C: Eq: Semiring: ClassTag](lhs: PolySparse[C], rhs: PolySparse[C]): PolySparse[C] = {
    val PolySparse(lexp, lcoeff) = lhs
    val PolySparse(rexp, rcoeff) = rhs

    val len = countSumTerms(lhs, rhs)
    val es = new Array[Int](len)
    val cs = new Array[C](len)

    @tailrec
    def sum(i: Int, j: Int, k: Int): PolySparse[C] =
      if (i < lexp.length && j < rexp.length) {
        val ei = lexp(i)
        val ej = rexp(j)
        if (ei == ej) {
          es(k) = ei
          cs(k) = lcoeff(i) + rcoeff(j)
          sum(i + 1, j + 1, k + 1)
        } else if (ei < ej) {
          es(k) = ei
          cs(k) = lcoeff(i)
          sum(i + 1, j, k + 1)
        } else {
          es(k) = ej
          cs(k) = rcoeff(j)
          sum(i, j + 1, k + 1)
        }
      } else {
        var k0 = k
        cfor(i)(_ < lexp.length, _ + 1) { i0 =>
          es(k0) = lexp(i0)
          cs(k0) = lcoeff(i0)
          k0 += 1
        }
        cfor(j)(_ < rexp.length, _ + 1) { j0 =>
          es(k0) = rexp(j0)
          cs(k0) = rcoeff(j0)
          k0 += 1
        }
        PolySparse.safe(es, cs)
      }

    sum(0, 0, 0)
  }

  private final def subtractScaled[C: Eq: Rng: ClassTag]
      (lhs: PolySparse[C], c: C, e: Int, rhs: PolySparse[C]) = {
    val PolySparse(lexp, lcoeff) = lhs
    val PolySparse(rexp, rcoeff) = rhs

    val len = countSumTerms(lhs, rhs, 0, e)
    val es = new Array[Int](len)
    val cs = new Array[C](len)

    @tailrec
    def loop(i: Int, j: Int, k: Int): PolySparse[C] = {
      if (i < lexp.length && j < rexp.length) {
        val ei = lexp(i)
        val ej = rexp(j) + e
        if (ei == ej) {
          es(k) = ei
          cs(k) = lcoeff(i) - c * rcoeff(j)
          loop(i + 1, j + 1, k + 1)
        } else if (ei < ej) {
          es(k) = ei
          cs(k) = lcoeff(i)
          loop(i + 1, j, k + 1)
        } else {
          es(k) = ej
          cs(k) = -c * rcoeff(j)
          loop(i, j + 1, k + 1)
        }
      } else {
        var k0 = k
        cfor(i)(_ < lexp.length, _ + 1) { i0 =>
          es(k0) = lexp(i0)
          cs(k0) = lcoeff(i0)
          k0 += 1
        }
        cfor(j)(_ < rexp.length, _ + 1) { j0 =>
          es(k0) = rexp(j0) + e
          cs(k0) = -c * rcoeff(j0)
          k0 += 1
        }
        PolySparse.safe(es, cs)
      }
    }

    loop(0, 0, 0)
  }

  private final def quotmodSparse[@spec(Double) C: Field: Eq: ClassTag]
      (lhs: PolySparse[C], rhs: PolySparse[C]): (PolySparse[C], PolySparse[C]) = {
    val rdegree = rhs.degree
    val rmaxCoeff = rhs.maxOrderTermCoeff

    @tailrec
    def inflate(ts: List[Term[C]], i: Int, es: Array[Int], cs: Array[C]): PolySparse[C] =
      ts match {
        case Term(c, e) :: ts0 => es(i) = e; cs(i) = c; inflate(ts0, i + 1, es, cs)
        case Nil => new PolySparse(es, cs)
      }

    @tailrec
    def loop(quot: List[Term[C]], rem: PolySparse[C]): (PolySparse[C], PolySparse[C]) =
      if (!rem.isZero && rem.degree >= rdegree) {
        val c0 = rem.maxOrderTermCoeff / rmaxCoeff
        val e0 = rem.degree - rdegree
        loop(Term(c0, e0) :: quot, subtractScaled(rem, c0, e0, rhs))
      } else {
        val len = quot.size
        (inflate(quot, 0, new Array[Int](len), new Array[C](len)), rem)
      }

    loop(Nil, lhs)
  }
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//PolyDense

class PolyDense[@spec(Double) C] private[spire] (val coeffs: Array[C])
    (implicit val ct: ClassTag[C]) extends Polynomial[C] { lhs =>

  def degree: Int = if (isZero) 0 else coeffs.length - 1

  def toSparse(implicit ring: Semiring[C], eq: Eq[C]): PolySparse[C] =
    Polynomial.sparse(data)

  def toDense(implicit ring: Semiring[C], eq: Eq[C]): PolyDense[C] = lhs

  def foreach[U](f: (Int, C) => U): Unit = {
    cfor(0)(_ < coeffs.length, _ + 1) { e =>
      f(e, coeffs(e))
    }
  }

  override def foreachNonZero[U](f: (Int, C) => U)(implicit ring: Semiring[C], eq: Eq[C]): Unit = {
    cfor(0)(_ < coeffs.length, _ + 1) { e =>
      val c = coeffs(e)
      if (c =!= ring.zero)
        f(e, c)
    }
  }

  def coeffsArray(implicit ring: Semiring[C]): Array[C] = coeffs

  def nth(n: Int)(implicit ring: Semiring[C]): C =
    if (n < coeffs.length) coeffs(n) else ring.zero

  def maxOrderTermCoeff(implicit ring: Semiring[C]): C =
    if (isZero) ring.zero else coeffs(degree)

  def reductum(implicit e: Eq[C], ring: Semiring[C], ct: ClassTag[C]): Polynomial[C] = {
    var i = coeffs.length - 2
    while (i >= 0 && coeffs(i) === ring.zero) i -= 1
    if (i < 0) {
      new PolyDense(new Array[C](0))
    } else {
      val arr = new Array[C](i + 1)
      System.arraycopy(coeffs, 0, arr, 0, i + 1)
      new PolyDense(arr)
    }
  }

  def isZero: Boolean =
    coeffs.length == 0

  def apply(x: C)(implicit ring: Semiring[C]): C = {
    if (isZero) return ring.zero

    var even = coeffs.length - 1
    var odd = coeffs.length - 2
    if ((even & 1) == 1) { even = odd; odd = coeffs.length - 1 }

    var c0 = coeffs(even)
    val x2 = x.pow(2)
    cfor(even - 2)(_ >= 0, _ - 2) { i =>
      c0 = coeffs(i) + c0 * x2
    }

    if (odd >= 1) {
      var c1 = coeffs(odd)
      cfor(odd - 2)(_ >= 1, _ - 2) { i =>
        c1 = coeffs(i) + c1 * x2
      }
      c0 + c1 * x
    } else {
      c0
    }
  }

  def derivative(implicit ring: Ring[C], eq: Eq[C]): Polynomial[C] = {
    if (isZero) return this
    val cs = new Array[C](degree)
    var j = coeffs.length - 1
    cfor(cs.length - 1)(_ >= 0, _ - 1) { i =>
      cs(i) = ring.fromInt(j) * coeffs(j)
      j -= 1
    }
    Polynomial.dense(cs)
  }

  def integral(implicit field: Field[C], eq: Eq[C]): Polynomial[C] = {
    val cs = new Array[C](coeffs.length + 1)
    cs(0) = field.zero
    cfor(0)(_ < coeffs.length, _ + 1) { i => cs(i + 1) = coeffs(i) / field.fromInt(i + 1) }
    Polynomial.dense(cs)
  }

  def unary_-()(implicit ring: Rng[C]): Polynomial[C] = {
    val negArray = new Array[C](coeffs.length)
    cfor(0)(_ < coeffs.length, _ + 1) { i => negArray(i) = -coeffs(i) }
    new PolyDense(negArray)
  }

  def +(rhs: Polynomial[C])(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] =
    PolyDense.plusDense(lhs, rhs)

  def *(rhs: Polynomial[C])(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] = {
    if (rhs.isZero) return rhs
    if (lhs.isZero) return lhs
    val lcs = lhs.coeffsArray
    val rcs = rhs.coeffsArray
    val cs = new Array[C](lcs.length + rcs.length - 1)
    cfor(0)(_ < cs.length, _ + 1) { i => cs(i) = ring.zero }
    cfor(0)(_ < lcs.length, _ + 1) { i =>
      val c = lcs(i)
      var k = i
      cfor(0)(_ < rcs.length, _ + 1) { j =>
        cs(k) += c * rcs(j)
        k += 1
      }
    }
    Polynomial.dense(cs)
  }

  def /%(rhs: Polynomial[C])(implicit field: Field[C], eq: Eq[C]): (Polynomial[C], Polynomial[C]) = {
    def zipSum(lcs: Array[C], rcs: Array[C])(implicit r: Ring[C]): Array[C] =
      (lcs + rcs).tail

    def polyFromCoeffsLE(cs: Array[C]): Polynomial[C] =
      Polynomial.dense(cs)

    def polyFromCoeffsBE(cs: Array[C]): Polynomial[C] = {
      val ncs = cs.dropWhile(_ === field.zero)
      Polynomial.dense(ncs.reverse)
    }
            
    @tailrec def eval(q: Array[C], u: Array[C], n: Int): (Polynomial[C], Polynomial[C]) = {
      if (u.isEmpty || n < 0) {
        (polyFromCoeffsLE(q), polyFromCoeffsBE(u))
      } else {
        val v0 = if (rhs.isZero) field.zero else rhs.maxOrderTermCoeff
        val q0 = try {
          val q0 = u(0) / v0
          q0
        } catch {
          case e: Exception =>
            println("%s %s" format (rhs.isZero, v0))
            println("%s / %s exploded" format (u(0), v0))
            throw e
        }
        val uprime = zipSum(u, rhs.coeffsArray.reverse.map(_ * -q0))
        eval(Array(q0) ++ q, uprime, n - 1)
      }
    }

    val cs = rhs.coeffsArray
    if (cs.length == 0) {
      throw new ArithmeticException("/ by zero polynomial")
    } else if (cs.length == 1) {
      val c = cs(0)
      val q = Polynomial.dense(lhs.coeffs.map(_ / c))
      val r = Polynomial.dense(new Array[C](0))
      (q, r)
    } else {
      eval(new Array[C](0), lhs.coeffs.reverse, lhs.degree - rhs.degree)
    }
  }

  def *: (k: C)(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] =
    if (k === ring.zero) {
      Polynomial.dense(new Array[C](0))
    } else {
      val cs = new Array[C](coeffs.length)
      cfor(0)(_ < cs.length, _ + 1) { i =>
        cs(i) = k * coeffs(i)
      }
      Polynomial.dense(cs)
    }
}

object PolyDense {
  private final def plusDense[C: Semiring: Eq: ClassTag](lhs: Polynomial[C], rhs: Polynomial[C]): Polynomial[C] = {
    val lcoeffs = lhs.coeffsArray
    val rcoeffs = rhs.coeffsArray
    if (lcoeffs.length < rcoeffs.length) {
      plusDense(rhs, lhs)
    } else {
      val cs = new Array[C](lcoeffs.length)
      cfor(0)(_ < rcoeffs.length, _ + 1) { i =>
        cs(i) = lcoeffs(i) + rcoeffs(i)
      }
      cfor(rcoeffs.length)(_ < lcoeffs.length, _ + 1) { i =>
        cs(i) = lcoeffs(i)
      }
      Polynomial.dense(cs)
    }
  }
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//Polynomial

object Polynomial extends PolynomialInstances {

  def dense[@spec(Double) C: Semiring: Eq: ClassTag](coeffs: Array[C]): PolyDense[C] = {
    var i = coeffs.length
    while (i > 0 && (coeffs(i - 1) == Semiring[C].zero)) i -= 1
    if (i == coeffs.length) {
      new PolyDense(coeffs)
    } else {
      val cs = new Array[C](i)
      System.arraycopy(coeffs, 0, cs, 0, i)
      new PolyDense(cs)
    }
  }

  def sparse[@spec(Double) C: Semiring: Eq: ClassTag](data: Map[Int, C]): PolySparse[C] =
    PolySparse(data)

  def apply[@spec(Double) C: Semiring: Eq: ClassTag](data: Map[Int, C]): PolySparse[C] =
    sparse(data)

  def apply[@spec(Double) C: Semiring: Eq: ClassTag](terms: Iterable[Term[C]]): PolySparse[C] =
    sparse(terms.map(_.toTuple)(collection.breakOut))

  def apply[@spec(Double) C: Semiring: Eq: ClassTag](c: C, e: Int): PolySparse[C] =
    PolySparse.safe(Array(e), Array(c))

  import scala.util.{Try, Success, Failure}

  def apply(s: String): Polynomial[Rational] = parse(s)

  def zero[@spec(Double) C: Eq: Semiring: ClassTag]: Polynomial[C] =
    PolySparse.zero[C]
  def constant[@spec(Double) C: Eq: Semiring: ClassTag](c: C): Polynomial[C] =
    if (c == Semiring[C].zero) zero[C] else Polynomial(Map(0 -> c))
  def linear[@spec(Double) C: Eq: Semiring: ClassTag](c: C): Polynomial[C] =
    if (c == Semiring[C].zero) zero[C] else Polynomial(Map(1 -> c))
  def quadratic[@spec(Double) C: Eq: Semiring: ClassTag](c: C): Polynomial[C] =
    if (c == Semiring[C].zero) zero[C] else Polynomial(Map(2 -> c))
  def cubic[@spec(Double) C: Eq: Semiring: ClassTag](c: C): Polynomial[C] =
    if (c == Semiring[C].zero) zero[C] else Polynomial(Map(3 -> c))
  def one[@spec(Double) C: Eq: Rig: ClassTag]: Polynomial[C] =
    constant(Rig[C].one)
  def x[@spec(Double) C: Eq: Rig: ClassTag]: Polynomial[C] =
    linear(Rig[C].one)
  def twox[@spec(Double) C: Eq: Rig: ClassTag]: Polynomial[C] =
    linear(Rig[C].one + Rig[C].one)

  private val termRe = "([0-9]+\\.[0-9]+|[0-9]+/[0-9]+|[0-9]+)?(?:([a-z])(?:\\^([0-9]+))?)?".r

  private val operRe = " *([+-]) *".r

  private def parse(s: String): Polynomial[Rational] = {

    // represents a term, plus a named variable v
    case class T(c: Rational, v: String, e: Int)

    // parse all the terms and operators out of the string
    @tailrec def parse(s: String, ts: List[T]): List[T] =
      if (s.isEmpty) {
        ts
      } else {
        val (op, s2) = operRe.findPrefixMatchOf(s) match {
          case Some(m) => (m.group(1), s.substring(m.end))
          case None => if (ts.isEmpty) ("+", s) else throw new IllegalArgumentException(s)
        }

        val m2 = termRe.findPrefixMatchOf(s2).getOrElse(throw new IllegalArgumentException(s2))
        val c0 = Option(m2.group(1)).getOrElse("1")
        val c = if (op == "-") "-" + c0 else c0
        val v = Option(m2.group(2)).getOrElse("")
        val e0 = Option(m2.group(3)).getOrElse("")
        val e = if (e0 != "") e0 else if (v == "") "0" else "1"

        val t = try {
          T(Rational(c), v, e.toInt)
        } catch {
          case _: Exception => throw new IllegalArgumentException(s"illegal term: $c*x^$e")
        }
        parse(s2.substring(m2.end), if (t.c == 0) ts else t :: ts)
      }

    // do some pre-processing to remove whitespace/outer parens
    val t = s.trim
    val u = if (t.startsWith("(") && t.endsWith(")")) t.substring(1, t.length - 1) else t

    // parse out the terms
    val ts = parse(u, Nil)

    // make sure we have at most one variable
    val vs = ts.view.map(_.v).toSet.filter(_ != "")
    if (vs.size > 1) throw new IllegalArgumentException("only univariate polynomials supported")

    // we're done!
    Polynomial(ts.map(t => (t.e, t.c)).toMap)
  }

  final def split[@spec(Double) C: ClassTag](poly: Polynomial[C]): (Array[Int], Array[C]) = {
    val es = ArrayBuilder.make[Int]()
    val cs = ArrayBuilder.make[C]()
    poly foreach { (e, c) =>
      es += e
      cs += c
    }
    (es.result(), cs.result())
  }

  def interpolate[C: Field: Eq: ClassTag](points: (C, C)*): Polynomial[C] = {
    def loop(p: Polynomial[C], xs: List[C], pts: List[(C, C)]): Polynomial[C] =
      pts match {
        case Nil =>
          p
        case (x, y) :: tail =>
          val c = Polynomial.constant((y - p(x)) / xs.map(x - _).qproduct)
          val prod = xs.foldLeft(Polynomial.one[C]) { (prod, xn) =>
            prod * (Polynomial.x[C] - constant(xn))
          }
          loop(p + c * prod, x :: xs, tail)
      }
    loop(Polynomial.zero[C], Nil, points.toList)
  }
}

trait Polynomial[@spec(Double) C] { lhs =>
  implicit def ct: ClassTag[C]

  /** Returns a polynmial that has a dense representation. */
  def toDense(implicit ring: Semiring[C], eq: Eq[C]): PolyDense[C]

  /** Returns a polynomial that has a sparse representation. */
  def toSparse(implicit ring: Semiring[C], eq: Eq[C]): PolySparse[C]

  def foreach[U](f: (Int, C) => U): Unit

  def foreachNonZero[U](f: (Int, C) => U)(implicit ring: Semiring[C], eq: Eq[C]): Unit =
    foreach { (e, c) => if (c =!= ring.zero) f(e, c) }

  /**
   * Returns the coefficients in little-endian order. So, the i-th element is
   * coeffsArray(i) * (x ** i).
   */
  def coeffsArray(implicit ring: Semiring[C]): Array[C]

  /**
   * Returns a list of non-zero terms.
   */
  def terms(implicit ring: Semiring[C], eq: Eq[C]): List[Term[C]] = {
    val lb = new scala.collection.mutable.ListBuffer[Term[C]]
    foreachNonZero { (e, c) =>
      lb += Term(c, e)
    }
    lb.result()
  }

  /** Returns a map from exponent to coefficient of this polynomial. */
  def data(implicit ring: Semiring[C], eq: Eq[C]): Map[Int, C] = {
    val bldr = new scala.collection.mutable.MapBuilder[Int, C, Map[Int, C]](Map.empty[Int, C])
    foreachNonZero { (e, c) =>
      bldr += ((e, c))
    }
    bldr.result()
  }

  /** Returns the coefficient of the n-th degree term. */
  def nth(n: Int)(implicit ring: Semiring[C]): C

  /** Returns the term of the highest degree in this polynomial. */
  def maxTerm(implicit ring: Semiring[C]): Term[C] = Term(maxOrderTermCoeff, degree)

  /** Returns the degree of this polynomial. */
  def degree: Int

  /** Returns the coefficient of max term of this polynomial. */
  def maxOrderTermCoeff(implicit ring: Semiring[C]): C

  /** Returns a polynomial with the max term removed. */
  def reductum(implicit e: Eq[C], ring: Semiring[C], ct: ClassTag[C]): Polynomial[C]

  /** Returns `true` if this polynomial is `ring.zero`. */
  def isZero: Boolean

  /** Evaluate the polynomial at `x`. */
  def apply(x: C)(implicit r: Semiring[C]): C

  /** Compose this polynomial with another. */
  def compose(y: Polynomial[C])(implicit ring: Rig[C], eq: Eq[C]): Polynomial[C] = {
    var polynomial: Polynomial[C] = Polynomial.zero[C]
    foreachNonZero { (e, c) =>
      val z: Polynomial[C] = y.pow(e) :* c
      polynomial = polynomial + z
    }
    polynomial
  }

  /**
   * Returns this polynomial as a monic polynomial, where the leading
   * coefficient (ie. `maxOrderTermCoeff`) is 1.
   */
  def monic(implicit f: Field[C], eq: Eq[C]): Polynomial[C] = this :/ maxOrderTermCoeff

  def derivative(implicit ring: Ring[C], eq: Eq[C]): Polynomial[C]
  def integral(implicit field: Field[C], eq: Eq[C]): Polynomial[C]

  // EuclideanRing ops.

  def unary_-()(implicit ring: Rng[C]): Polynomial[C]
  def +(rhs: Polynomial[C])(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C]
  def -(rhs: Polynomial[C])(implicit ring: Rng[C], eq: Eq[C]): Polynomial[C] = lhs + (-rhs)
  def *(rhs: Polynomial[C])(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C]
  def /~(rhs: Polynomial[C])(implicit field: Field[C], eq: Eq[C]): Polynomial[C] = (lhs /% rhs)._1
  def /%(rhs: Polynomial[C])(implicit field: Field[C], eq: Eq[C]): (Polynomial[C], Polynomial[C])
  def %(rhs: Polynomial[C])(implicit field: Field[C], eq: Eq[C]): Polynomial[C] = (lhs /% rhs)._2

  def **(k: Int)(implicit ring: Rig[C], eq: Eq[C]): Polynomial[C] = pow(k)

  def pow(k: Int)(implicit ring: Rig[C], eq: Eq[C]): Polynomial[C] = {
    def loop(b: Polynomial[C], k: Int, extra: Polynomial[C]): Polynomial[C] =
      if (k == 1)
        b * extra
      else
        loop(b * b, k >>> 1, if ((k & 1) == 1) b * extra else extra)

    if (k < 0) {
      throw new IllegalArgumentException("negative exponent")
    } else if (k == 0) {
      Polynomial.one[C]
    } else if (k == 1) {
      this
    } else {
      loop(this, k - 1, this)
    }
  }

  // VectorSpace ops.

  def *: (k: C)(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C]
  def :* (k: C)(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] = k *: lhs
  def :/ (k: C)(implicit field: Field[C], eq: Eq[C]): Polynomial[C] = this :* k.reciprocal

  override def equals(that: Any): Boolean = that match {
    case rhs: Polynomial[_] if lhs.degree == rhs.degree =>
      val (les, lcs) = Polynomial.split(lhs)
      val (res, rcs) = Polynomial.split[Any](rhs.asInstanceOf[Polynomial[Any]])

      @tailrec
      def loop(i: Int, j: Int): Boolean = {
        if (i >= les.length && j >= res.length) {
          true
        } else if (j >= res.length || les(i) < res(j)) {
          if (lcs(i) == 0) loop(i + 1, j) else false
        } else if (i >= les.length || les(i) > res(j)) {
          if (rcs(j) == 0) loop(i, j + 1) else false
        } else if (lcs(i) == rcs(j)) {
          loop(i + 1, j + 1)
        } else {
          false
        }
      }

      loop(0, 0)

    case rhs: Polynomial[_] =>
      false

    case n if lhs.isZero =>
      n == 0

    case n if lhs.degree == 0 =>
      val (_, lcs) = Polynomial.split(lhs)
      lcs(0) == n

    case _ =>
      false
  }

  override def toString =
    if (isZero) {
      "(0)"
    } else {
      val bldr = ArrayBuilder.make[Term[C]]()
      foreach { (e, c) => bldr += Term(c, e) }

      val ts = bldr.result()
      QuickSort.sort(ts)(Order[Term[C]].reverse, implicitly[ClassTag[Term[C]]])
      val s = ts.mkString
      "(" + (if (s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)) + ")"
    }
}

trait PolynomialSemiring[@spec(Double) C]
extends Semiring[Polynomial[C]] {
  implicit def scalar: Semiring[C]
  implicit def eq: Eq[C]
  implicit def ct: ClassTag[C]

  def zero: Polynomial[C] = Polynomial.zero[C]
  def plus(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = x + y
  def times(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = x * y
}

trait PolynomialRig[@spec(Double) C] extends PolynomialSemiring[C]
with Rig[Polynomial[C]] {
  implicit override val scalar: Rig[C]

  def one: Polynomial[C] = Polynomial.one[C]
}

trait PolynomialRng[@spec(Double) C] extends PolynomialSemiring[C]
with RingAlgebra[Polynomial[C], C] {
  implicit override val scalar: Rng[C]

  def timesl(r: C, v: Polynomial[C]): Polynomial[C] = r *: v
  def negate(x: Polynomial[C]): Polynomial[C] = -x
}

trait PolynomialRing[@spec(Double) C] extends PolynomialRng[C]
with Ring[Polynomial[C]] {
  implicit override val scalar: Ring[C]

  def one: Polynomial[C] = Polynomial.one[C]
}

trait PolynomialEuclideanRing[@spec(Double) C] extends PolynomialRing[C]
with EuclideanRing[Polynomial[C]] with VectorSpace[Polynomial[C], C] {
  implicit override val scalar: Field[C]

  override def divr(x: Polynomial[C], k: C): Polynomial[C] = x :/ k
  def quot(x: Polynomial[C], y: Polynomial[C]) = x /~ y
  def mod(x: Polynomial[C], y: Polynomial[C]) = x % y
  override def quotmod(x: Polynomial[C], y: Polynomial[C]) = x /% y

  final def gcd(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = {
    val k = math.gcd(x.coeffsArray ++ y.coeffsArray)
    k *: euclid(x :/ k, y :/ k)(Polynomial.eq).monic
  }
}

trait PolynomialEq[@spec(Double) C] extends Eq[Polynomial[C]] {
  implicit def scalar: Semiring[C]
  implicit def eq: Eq[C]
  implicit def ct: ClassTag[C]

  def eqv(x: Polynomial[C], y: Polynomial[C]): Boolean =
    x.coeffsArray == y.coeffsArray // TODO: This is bad for sparse arrays. Do better.
}

trait PolynomialInstances0 {
  implicit def semiring[@spec(Double) C: ClassTag: Semiring: Eq] =
    new PolynomialSemiring[C] {
      val scalar = Semiring[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }

  implicit def eq[@spec(Double) C: ClassTag: Semiring: Eq] =
    new PolynomialEq[C] {
      val scalar = Semiring[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait PolynomialInstances1 extends PolynomialInstances0 {
  implicit def rig[@spec(Double) C: ClassTag: Rig: Eq] =
    new PolynomialRig[C] {
      val scalar = Rig[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }

  implicit def rng[@spec(Double) C: ClassTag: Rng: Eq] =
    new PolynomialRng[C] {
      val scalar = Rng[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait PolynomialInstances2 extends PolynomialInstances1 {
  implicit def ring[@spec(Double) C: ClassTag: Ring: Eq] =
    new PolynomialRing[C] {
      val scalar = Ring[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait PolynomialInstances3 extends PolynomialInstances2 {
  implicit def euclideanRing[@spec(Double) C: ClassTag: Field: Eq] =
    new PolynomialEuclideanRing[C] {
      val scalar = Field[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait PolynomialInstances extends PolynomialInstances3


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//Rational

sealed abstract class Rational extends ScalaNumber with ScalaNumericConversions with Ordered[Rational] { lhs =>
  import LongRationals.LongRational
  import BigRationals.BigRational

  def numerator: BigInt
  def denominator: BigInt

  def numeratorAsLong: Long
  def denominatorAsLong: Long

  def isWhole: Boolean

  // ugh, ScalaNumber and ScalaNumericConversions in 2.10 require this hack
  override def underlying: Object = this

  def abs: Rational = if (this < Rational.zero) -this else this
  def inverse: Rational = Rational.one / this
  def reciprocal: Rational
  def signum: Int = numerator.signum

  def unary_-(): Rational = Rational.zero - this

  def +(rhs: Rational): Rational
  def -(rhs: Rational): Rational
  def *(rhs: Rational): Rational
  def /(rhs: Rational): Rational

  def /~(rhs: Rational): Rational = Rational(SafeLong((this / rhs).toBigInt), SafeLong.one)
  def %(rhs: Rational): Rational = this - (this /~ rhs) * rhs
  def /%(rhs: Rational): (Rational, Rational) = {
    val q = this /~ rhs
    (q, this - q * rhs)
  }

  def gcd(rhs: Rational): Rational

  def toBigInt: BigInt
  def toBigDecimal: BigDecimal
  override def shortValue = longValue.toShort
  override def byteValue = longValue.toByte

  def floor: Rational
  def ceil: Rational
  def round: Rational

  def roundTo(denom: SafeLong): Rational = (this * denom).round / denom

  def pow(exp: Int): Rational

  /**
   * Returns this `Rational` to the exponent `exp`. Both the numerator and
   * denominator of `exp` must be valid integers. Anything larger will cause
   * `pow` to throw an `ArithmeticException`.
   */
  def pow(exp: Rational)(implicit ctxt: ApproximationContext[Rational]): Rational = {
    if (exp < 0) {
      this.inverse.pow(-exp)(ctxt)
    } else if (!(exp.numerator.isValidInt) || !(exp.denominator.isValidInt)) {
      throw new ArithmeticException("Exponent is too large!")
    } else {
      
      // nroot must be done last so the context is still valid, otherwise, we'd
      // need to adjust the error, as the absolute error would increase,
      // relatively, by (1 + e)^exp.numerator if nroot was done before the pow.

      (this pow exp.numerator.toInt).nroot(exp.denominator.toInt)(ctxt)
    }
  }


  /**
   * Find the n-th root of this `Rational`. This requires an (implicit)
   * `ApproximationContext` to bound the allowable absolute error of the answer.
   */
  def nroot(k: Int)(implicit ctxt: ApproximationContext[Rational]): Rational = if (k == 0) {
    Rational.one
  } else if (k < 0) {
    this.inverse.nroot(-k)(ctxt)
  } else if (numerator == 0) {
    Rational.zero
  } else {

    // TODO: Is this necessary with the better init. approx in the else?

    val (low, high) = this match {
      case LongRational(n, d) => {
        val n_ = Rational.nroot(n, k)
        val d_ = Rational.nroot(d, k)
        (Rational(n_._1, d_._2), Rational(n_._2, d_._1))
      }
      case BigRational(n, d) => {
        val n_ = Rational.nroot(n, k)
        val d_ = Rational.nroot(d, k)
        (Rational(n_._1, d_._2), Rational(n_._2, d_._1))
      }
    }
    
    if (low == high) {
      low
    } else {
    
      import Rational.{ nroot => intNroot }

      // To ensure the initial approximation is within (relatively) 1/n of the
      // actual root, we need to ensure the num. and den. are both >= min.
      // Otherwise, we need to find a single multiplier for them that can
      // guarantee this. From there, we can simply use the integer version of
      // nroot to get a good approximation.

      val min = (BigInt(k) * 2 + 1) pow k
      val mul = min / (this.numerator min this.denominator) + 1
      val numIntRt = intNroot(numerator * mul, k)
      val denIntRt = intNroot(denominator * mul, k)
      val low = Rational(numIntRt._1, denIntRt._2)
      val high = Rational(numIntRt._2, denIntRt._1)

      // Reduction in absolute error from n-th root algorithm:
      // Let x(k) be the approximation at step k, x(oo) be the n-th root. Let
      // e(k) be the relative error at step k, thus x(k) = x(oo)(1 + e(k)). If
      // x(0) > x(oo), then x(k) > x(oo) (this can be seen during the
      // derivation).
      //
      // x(k+1) = 1/n [(n-1) * x(k) + x(oo)^n / x(k)^(n-1)]
      //          1/n [(n-1) * x(oo) * (1 + e(k)) + x(oo)^n / (x(oo) * (1 + e(k)))^(n-1)]
      //          x(oo)[(n-1)*(1+e(k)) / n + 1 / (n * (1 + e(k))^(n-1))]
      //          x(oo)[1 + e(k) + (1 + e(k))/n + 1 / (n * (1 + e(k))^(n-1))]
      //          x(oo)[1 + e(k) * (1 - ((1 + e(k))^n - 1) / (e(k) * n * (1 + e(k))^(n-1))]
      //          x(oo)[1 + e(k) * (1 - ((1 + n*e(k) + nC2*e(k)^2 + ... + e(k)^n) - 1) / (.. as above ..))]
      //          x(oo)[1 + e(k) * (1 - (1 + nC2*e^2/n + ... + e^(n-1)/n) / (1 + e(k))^(n-1))]
      //        < x(oo)[1 + e(k) * (1 - 1 / (1 + e(k))^(n-1))]
      //        < x(oo)[1 + e(k) * (1 - 1 / (1 + 1/n)^(n-1))]
      // Let e = (1 + 1/n)^(n-1).
      //        < x(oo)[1 + e(k) * (e - 1 / e)]
      //          
      // So, we use a = (e - 1) / e as the relative error multiplier.

      val e = Rational(k + 1, k) pow (k - 1)
      val a = (e - 1) / e   // The relative error is multiplied by this each iter.
      val error = ctxt.error
      val absErr = high - low // I have no idea why I had this: high / k

      // absErr * a^k < error => a^k = error / absErr => k = log(error / absErr) / log(a)
      val maxiters = math.ceil(math.log((error / absErr).toDouble) / math.log(a.toDouble)).toInt
      
      // A single step of the nth-root algorithm.
      @inline def refine(x: Rational) = (x * (k - 1) + this / (x pow (k - 1))) / k

      def findNthRoot(prev: Rational, i: Int): Rational = if (i == maxiters) {
        prev
      } else {
        val next = refine(prev)
        
        // We know x(e0 - e1) > (1 - a)xe0, so xe0 < x(e0 - e1) / (1 - a).
        // Thus, if we have the difference, we can recalculate our guess of the
        // absolute error more "accurately" by dividing the difference of the
        // previous 2 guesses by (1 - a).
        //
        // This recalculation helps a lot. The numbers are a lot saner.

        // TODO: If we remove the iters constraint and just use this, will we
        //       ever perform worse than iters + 1 iterations? Need proof.

        if (prev == next || ((prev - next) / (Rational(1) - a)) < error) {
          prev
        } else {
          findNthRoot(next, i + 1)
        }
      }

      findNthRoot(high, 0)
    }
  }

  def sign: Sign = Sign(signum)

  def compareToOne: Int

  def min(rhs: Rational): Rational =
    if ((lhs compare rhs) < 0) lhs else rhs

  def max(rhs: Rational): Rational =
    if ((lhs compare rhs) > 0) lhs else rhs

  /**
   * Returns a `Rational` whose numerator and denominator both fit in an `Int`.
   */
  def limitToInt: Rational = limitTo(BigInt(Int.MaxValue))


  /**
   * Returns a `Rational` whose numerator and denominator both fit in a `Long`.
   */
  def limitToLong: Rational = limitTo(BigInt(Long.MaxValue))


  /**
   * Returns a `Rational` whose denominator and numerator are no larger than
   * `max` and whose value is close to the original. This applies, even if, for
   * example, this `Rational` is greater than `max`. In that case,
   * `Rational(max, 1)` is returned.
   *
   * @param max A positive integer.
   */
  def limitTo(max: BigInt): Rational = if (this.signum < 0) {
    -((-this).limitTo(max))
  } else {
    require(max > 0, "Limit must be a positive integer.")

    val half = max >> 1
    val floor = this.toBigInt
    if (floor >= max) {
      Rational(max)
    } else if (floor >= (max >> 1)) {
      Rational(floor.toLong)
    } else if (this < Rational(1)) {
      limitDenominatorTo(max)
    } else {
      limitDenominatorTo(max * denominator / numerator)
    }
  }


  /**
   * Finds the closest `Rational` to this `Rational` whose denominator is no
   * larger than `limit`.
   *
   * See [[http://en.wikipedia.org/wiki/Stern%E2%80%93Brocot_tree#Mediants_and_binary_search]]
   */
  def limitDenominatorTo(limit: BigInt): Rational = {
    require(limit > 0, "Cannot limit denominator to non-positive number.")

    // TODO: We should always perform a binary search from the left or right to
    //       speed up computation. For example, if in a search, we have a lower
    //       bound of 1/2 for many steps, then each step will only add 1/2 to
    //       the upper-bound, and so we'd converge on the number quite slowly.
    //       However, we can speed this up by tentatively checking if we could
    //       skip some intermediate values, by performing an adaptive search.
    //       We'd simply keep doubling the number of steps we're skipping until
    //       the upper-bound (eg) is now the lower bound, then go back to find
    //       the greatest lower bound in the steps we missed by binary search.
    //       Instead of adding 1/2 n times, we would try to add 1/2, 2/4, 4/8,
    //       8/16, etc., until the upper-bound swiches to a lower bound. Say
    //       this happens a (1/2)*2^k, then we simply perform a binary search in
    //       between (1/2)*2^(k-1) and (1/2)*2^k to find the new lower bound.
    //       This would reduce the number of steps to O(log n).

    @tailrec
    def closest(l: Rational, u: Rational): Rational = {
      val mediant = Rational(l.numerator + u.numerator, l.denominator + u.denominator)
    
      if (mediant.denominator > limit) {
        if ((this - l).abs > (u - this).abs) u else l
      } else if (mediant == this) {
        mediant
      } else if (mediant < this) {
        closest(mediant, u)
      } else {
        closest(l, mediant)
      }
    }

    this.sign match {
      case Zero => this
      case Positive => closest(Rational(this.toBigInt), LongRationals.LongRational(1, 0))
      case Negative => closest(LongRationals.LongRational(-1, 0), Rational(this.toBigInt))
    }
  }
}


object Rational extends RationalInstances {
  private val RationalString = """^(-?\d+)/(-?\d+)$""".r
  private val IntegerString = """^(-?\d+)$""".r

  import LongRationals.LongRational
  import BigRationals.BigRational

  val zero: Rational = LongRational(0L, 1L)
  val one: Rational = LongRational(1L, 1L)
  
  def apply(n: SafeLong, d: SafeLong): Rational = {
    if (d < 0) return apply(-n, -d)
    val g = n gcd d
    (n / g).foldWith[Rational,LongRational,BigRational](d / g)(LongRational(_, _), BigRational(_, _))
  }

  def apply(n: Long, d: Long): Rational = LongRationals.build(n, d)
  def apply(n: BigInt, d: BigInt): Rational = BigRationals.build(n, d)

  protected def unsafeBuild(n: Long, d: Long) = LongRationals.unsafeBuild(n, d)
  protected def unsafeBuild(n: BigInt, d: BigInt) = BigRationals.unsafeBuild(n, d)

  implicit def apply(x: Int): Rational = LongRationals.build(x, 1L)
  implicit def apply(x: Long): Rational = LongRationals.build(x, 1L)
  implicit def apply(x: BigInt): Rational = BigRationals.build(x, BigInt(1))

  implicit def apply(x: Float): Rational = apply(x.toDouble)

  implicit def apply(x: Double): Rational = {
    val bits = java.lang.Double.doubleToLongBits(x)
    val value = if ((bits >> 63) < 0) -(bits & 0x000FFFFFFFFFFFFFL | 0x0010000000000000L)
                else (bits & 0x000FFFFFFFFFFFFFL | 0x0010000000000000L)
    val exp = ((bits >> 52) & 0x7FF).toInt - 1075 // 1023 + 52
    if (exp > 10) {
        apply(BigInt(value) << exp, BigInt(1))
    } else if (exp >= 0) {
      apply(value << exp, 1L)
    } else if (exp >= -52 && (~((-1L) << (-exp)) & value) == 0L) {
      apply(value >> (-exp), 1L)
    } else {
      apply(BigInt(value), BigInt(1) << (-exp))
    }
  }

  implicit def apply(x:BigDecimal): Rational = {
    if (x.ulp >= 1) {
      BigRationals.build(x.toBigInt, 1)
    } else {
      val n = (x / x.ulp).toBigInt
      val d = (BigDecimal(1.0) / x.ulp).toBigInt
      BigRationals.build(n, d)
    }
  }

  def apply(r: String): Rational = r match {
    case RationalString(n, d) => Rational(BigInt(n), BigInt(d))
    case IntegerString(n) => Rational(BigInt(n))
    case s => try {
      Rational(BigDecimal(s))
    } catch {
      case nfe: NumberFormatException => throw new NumberFormatException("For input string: " + s)
    }
  }

  implicit def apply(x: SafeLong): Rational =
    x.fold(LongRationals.unsafeBuild(_, 1L), BigRationals.unsafeBuild(_, BigInt(1)))

  implicit def apply(x: Number): Rational = x match {
    case RationalNumber(n) => apply(n)
    case IntNumber(n) => apply(n)
    case FloatNumber(n) => apply(n)
    case DecimalNumber(n) => apply(n)
  }

  /**
   * Returns an interval that bounds the nth-root of the integer x.
   *
   * TODO: This is really out-of-place too.
   */
  def nroot(x: BigInt, n: Int): (BigInt, BigInt) = {
    def findnroot(prev: BigInt, add: Int): (BigInt, BigInt) = {
      val min = prev setBit add
      val max = min + 1

      val fl = min pow n
      val cl = max pow n

      if (fl > x) {
        findnroot(prev, add - 1)
      } else if (cl < x) {
        findnroot(min, add - 1)
      } else if (cl == x) {
        (max, max)
      } else if (fl == x) {
        (min, min)
      } else {
        (min, max)
      }
    }

    findnroot(BigInt(0), (x.bitLength + n - 1) / n) // ceil(x.bitLength / n)
  }


  /**
   * Returns an interval that bounds the nth-root of the integer x.
   */
  def nroot(x: Long, n: Int): (Long, Long) = {
    def findnroot(prev: Long, add: Long): (Long, Long) = {
      val min = prev | add
      val max = min + 1
      val fl = pow(min, n)
      val cl = pow(max, n)

      if (fl <= 0 || fl > x) {
        findnroot(prev, add >> 1)
      } else if (cl < x) {
        findnroot(min, add >> 1)
      } else if (cl == x) {
        (max, max)
      } else if (fl == x) {
        (min, min)
      } else {
        (min, max)
      }
    }

    // TODO: Could probably get a better initial add then this.
    findnroot(0, 1L << ((65 - n) / n))
  }
}




private abstract class Rationals[@specialized(Long) A](implicit integral: Integral[A]) {
  import integral._

  def build(n: A, d: A): Rational

  //sealed trait RationalLike extends Rational with Fraction[A] {
  sealed trait RationalLike extends Rational {
    def num: A
    def den: A
    
    override def signum: Int = scala.math.signum(integral.compare(num, zero))

    def isWhole: Boolean = den == one

    def toBigInt: BigInt = (integral.toBigInt(num) / integral.toBigInt(den))
    def toBigDecimal: BigDecimal = integral.toBigDecimal(num) / integral.toBigDecimal(den)

    def longValue = toBigInt.longValue    // Override if possible.
    def intValue = longValue.intValue

    def floatValue = doubleValue.toFloat

    def doubleValue: Double = if (num == zero) {
      0.0
    } else if (integral.lt(num, zero)) {
      -((-this).toDouble)
    } else {

      // We basically just shift n so that integer division gives us 54 bits of
      // accuracy. We use the last bit for rounding, so end w/ 53 bits total.

      val n = integral.toBigInt(num)
      val d = integral.toBigInt(den)

      val sharedLength = Math.min(n.bitLength, d.bitLength)
      val dLowerLength = d.bitLength - sharedLength

      val nShared = n >> (n.bitLength - sharedLength)
      val dShared = d >> dLowerLength
    
      d.underlying.getLowestSetBit() < dLowerLength
      val addBit = if (nShared < dShared || (nShared == dShared && d.underlying.getLowestSetBit() < dLowerLength)) {
        1
      } else {
        0
      }

      val e = d.bitLength - n.bitLength + addBit
      val ln = n << (53 + e)    // We add 1 bit for rounding.
      val lm = (ln / d).toLong
      val m = ((lm >> 1) + (lm & 1)) & 0x000fffffffffffffL
      val bits = (m | ((1023L - e) << 52))
      java.lang.Double.longBitsToDouble(bits)
    }
      

    override def hashCode: Int =
      if (isWhole && toBigInt == toLong) unifiedPrimitiveHashcode
      else 29 * (37 * num.## + den.##)

    override def equals(that: Any): Boolean = that match {
      case that: Real => this == that.toRational
      case that: Algebraic => that == this
      case that: RationalLike => num == that.num && den == that.den
      case that: BigInt => isWhole && toBigInt == that
      case that: BigDecimal => try { toBigDecimal == that } catch { case ae: ArithmeticException => false }
      case that: SafeLong => SafeLong(toBigInt) == that
      case that: Number => Number(this) == that
      case that: Natural => isWhole && this == Rational(that.toBigInt)
      case that: Complex[_] => that == this
      case that: Quaternion[_] => that == this
      case that => unifiedPrimitiveEquals(that)
    }

    override def toString: String = if (den == 1L)
      num.toString
    else
      "%s/%s" format (num, den)
  }
}


private object LongRationals extends Rationals[Long] {
  import BigRationals.BigRational

  def build(n: Long, d: Long): Rational = {
    if (d == 0) throw new IllegalArgumentException("0 denominator")
    else if (d > 0) unsafeBuild(n, d)
    else if (n == Long.MinValue || d == Long.MinValue) Rational(-BigInt(n), -BigInt(d))
    else unsafeBuild(-n, -d)
  }

  def unsafeBuild(n: Long, d: Long): Rational = {
    if (n == 0L) return Rational.zero

    val divisor = spire.math.gcd(n, d)
    if (divisor == 1L) {
      if (d < 0)
        Rational(SafeLong(-n), SafeLong(-d))
      else
        LongRational(n, d)
    } else {
      if (d < 0)
        LongRational(-n / divisor, -d / divisor)
      else
        LongRational(n / divisor, d / divisor)
    }
  }

  @SerialVersionUID(0L)
  case class LongRational private (n: Long, d: Long) extends RationalLike with Serializable {
    def num: Long = n
    def den: Long = d

    def numerator = ConvertableFrom[Long].toBigInt(n)
    def denominator = ConvertableFrom[Long].toBigInt(d)

    def numeratorAsLong: Long = n
    def denominatorAsLong: Long = d

    def reciprocal =
      if (n == 0L) throw new ArithmeticException("reciprocal called on 0/1")
      else if (n > 0L) LongRational(d, n)
      else if (n == Long.MinValue || d == Long.MinValue) BigRational(-BigInt(d), -BigInt(n))
      else LongRational(-d, -n)

    override def signum: Int = java.lang.Long.signum(n)

    override def unary_-(): Rational =
      if (n == Long.MinValue) BigRational(-BigInt(Long.MinValue), BigInt(d))
      else LongRational(-n, d)

    def +(r: Rational): Rational = r match {
      case r: LongRationals.LongRational =>
        val dgcd: Long = math.gcd(d, r.d)
        if (dgcd == 1L) {

          val num = SafeLong(n) * r.d + SafeLong(r.n) * d
          val den = SafeLong(d) * r.d
          Rational(num, den)

        } else {

          val lden: Long = d / dgcd
          val rden: Long = r.d / dgcd
          val num: SafeLong = SafeLong(n) * rden + SafeLong(r.n) * lden
          val ngcd: Long = num.fold(spire.math.gcd(_, dgcd), num => spire.math.gcd(dgcd, (num % dgcd).toLong))
          if (ngcd == 1L)
            Rational(num, SafeLong(lden) * r.d)
          else
            Rational(num / ngcd, SafeLong(lden) * (r.d / ngcd))
        }
      case r: BigRational =>
        val dgcd: Long = spire.math.gcd(d, (r.d % d).toLong)
        if (dgcd == 1L) {

          val num = SafeLong(r.d * n + r.n * d)
          val den = SafeLong(r.d * d)
          Rational(num, den)

        } else {

          val lden: Long = d / dgcd
          val rden: SafeLong = SafeLong(r.d) / dgcd
          val num: SafeLong = rden * n + SafeLong(r.n) * lden
          val ngcd: Long = num.fold(spire.math.gcd(_, dgcd), num => spire.math.gcd(dgcd, (num % dgcd).toLong))
          if (ngcd == 1L)
            Rational(num, SafeLong(lden) * r.d)
          else
            Rational(num / ngcd, SafeLong(r.d / ngcd) * lden)

        }
    }


    def -(r: Rational): Rational = r match {
      case r: LongRationals.LongRational =>
        val dgcd: Long = spire.math.gcd(d, r.d)
        if (dgcd == 1L) {

          val num = SafeLong(n) * r.d - SafeLong(r.n) * d
          val den = SafeLong(d) * r.d
          Rational(num, den)

        } else {

          val lden: Long = d / dgcd
          val rden: Long = r.d / dgcd
          val num: SafeLong = SafeLong(n) * rden - SafeLong(r.n) * lden
          val ngcd: Long = num.fold(spire.math.gcd(_, dgcd), num => spire.math.gcd(dgcd, (num % dgcd).toLong))
          if (ngcd == 1L)
            Rational(num, SafeLong(lden) * r.d)
          else
            Rational(num / ngcd, SafeLong(lden) * (r.d / ngcd))
        }
      case r: BigRational =>
        val dgcd: Long = math.gcd(d, (r.d % d).toLong)
        if (dgcd == 1L) {

          val num = SafeLong(r.d * n - r.n * d)
          val den = SafeLong(r.d * d)
          Rational(num, den)

        } else {

          val lden: Long = d / dgcd
          val rden: SafeLong = SafeLong(r.d) / dgcd
          val num: SafeLong = rden * n - SafeLong(r.n) * lden
          val ngcd: Long = num.fold(spire.math.gcd(_, dgcd), num => spire.math.gcd(dgcd, (num % dgcd).toLong))
          if (ngcd == 1L)
            Rational(num, SafeLong(lden) * r.d)
          else
            Rational(num / ngcd, SafeLong(r.d / ngcd) * lden)

        }
    }


    def *(r: Rational): Rational = {
      if (n == 0L) Rational.zero else (r match {
        case r: LongRationals.LongRational =>
          val a = spire.math.gcd(n, r.d)
          val b = spire.math.gcd(d, r.n)
          Rational(SafeLong(n / a) * (r.n / b), SafeLong(d / b) * (r.d / a))
        case r: BigRational =>
          val a = spire.math.gcd(n, (r.d % n).toLong)
          val b = spire.math.gcd(d, (r.n % d).toLong)
          Rational(SafeLong(n / a) * (r.n / b), SafeLong(d / b) * (r.d / a))
      })
    }


    def /(r: Rational): Rational = {
      if (r == Rational.zero) throw new ArithmeticException("divide (/) by 0")
      if (this == Rational.zero) return this
      r match {
        case r: LongRationals.LongRational => {
          val a = spire.math.gcd(n, r.n)
          val b = spire.math.gcd(d, r.d)
          val num = SafeLong(n / a) * (r.d / b)
          val den = SafeLong(d / b) * (r.n / a)
          if (den < SafeLong.zero) Rational(-num, -den) else Rational(num, den)
        }
        case r: BigRational => {
          val a = spire.math.gcd(n, (r.n % n).toLong)
          val b = spire.math.gcd(d, (r.d % d).toLong)
          val num = SafeLong(n / a) * (r.d / b)
          val den = SafeLong(d / b) * (r.n / a)
          if (den < SafeLong.zero) Rational(-num, -den) else Rational(num, den)
        }
      }
    }

    def gcd(r: Rational): Rational = r match {
      case r: LongRationals.LongRational =>
        val dgcd: Long = spire.math.gcd(d, r.d)
        val n0 = spire.math.abs(n)
        val n1 = spire.math.abs(r.n)
        if (dgcd == 1L) {
          Rational(spire.math.gcd(n0, n1), SafeLong(d) * r.d)
        } else {
          val lm = d / dgcd
          val rm = r.d / dgcd
          Rational((SafeLong(n0) * rm) gcd (SafeLong(n1) * lm), SafeLong(dgcd) * lm * rm)
        }

      case r: BigRational =>
        val dgcd: Long = spire.math.gcd(d, (r.d % d).toLong)
        if (dgcd == 1L) {
          Rational(spire.math.gcd(spire.math.abs(n), spire.math.abs((r.n % n).toLong)),
            SafeLong(d) * r.d)
        } else {
          val lm = d / dgcd
          val rm = r.d / dgcd
          Rational((SafeLong(spire.math.abs(n)) * rm) gcd (SafeLong(r.n.abs) * lm),
            SafeLong(dgcd) * lm * rm)
        }
    }

    def floor: Rational =
      if (d == 1L) this
      else if (n >= 0) Rational(n / d, 1L)
      else Rational(n / d - 1L, 1L)

    def ceil: Rational =
      if (d == 1L) this
      else if (n >= 0) Rational(n / d + 1L, 1L)
      else Rational(n / d, 1L)

    def round: Rational =
      if (n >= 0) {
        val m = (n % d)
        if (m >= (d - m)) Rational(n / d + 1) else Rational(n / d)
      } else {
        val m = -(n % d)
        if (m >= (d - m)) Rational(n / d - 1) else Rational(n / d)
      }

    def pow(exp: Int): Rational = if (exp == 0)
      Rational.one
    else if (exp < 0)
      reciprocal.pow(-exp)
    else
      Rational(SafeLong(n).pow(exp), SafeLong(d).pow(exp))

    def compareToOne: Int = n compare d

    def compare(r: Rational): Int = r match {
      case r: LongRationals.LongRational =>
        val dgcd = spire.math.gcd(d, r.d)
        if (dgcd == 1L)
          (SafeLong(n) * r.d - SafeLong(r.n) * d).signum
        else
          (SafeLong(n) * (r.d / dgcd) - SafeLong(r.n) * (d / dgcd)).signum

      case r: BigRational =>
        val dgcd = spire.math.gcd(d, (r.d % d).toLong)
        if (dgcd == 1L)
          (SafeLong(n) * r.d - SafeLong(r.n) * d).signum
        else
          (SafeLong(n) * (r.d / dgcd) - SafeLong(r.n) * (d / dgcd)).signum
    }
  }
}


private object BigRationals extends Rationals[BigInt] {
  import LongRationals.LongRational

  def build(n: BigInt, d: BigInt): Rational = {
    if (d == 0) throw new IllegalArgumentException("0 denominator")
    else if (d > 0) unsafeBuild(n, d)
    else unsafeBuild(-n, -d)
  }

  def unsafeBuild(n: BigInt, d:BigInt): Rational = {
    if (n == 0) return Rational.zero

    val gcd = n.gcd(d)
    if (gcd == 1) {
      if (d < 0)
        Rational(SafeLong(-n), SafeLong(-d))
      else
        Rational(SafeLong(n), SafeLong(d))
    } else {
      if (d < 0)
        Rational(-SafeLong(n / gcd), -SafeLong(d / gcd))
      else
        Rational(SafeLong(n / gcd), SafeLong(d / gcd))
    }
  }


  @SerialVersionUID(0L)
  case class BigRational private (n: BigInt, d: BigInt) extends RationalLike with Serializable {
    def num: BigInt = n
    def den: BigInt = d

    def numerator = n
    def denominator = d

    def numeratorAsLong: Long = n.toLong
    def denominatorAsLong: Long = d.toLong

    def reciprocal = if (n == 0)
      throw new ArithmeticException("reciprocal called on 0/1")
    else if (n < 0)
      BigRational(-d, -n)
    else
      BigRational(d, n)

    override def signum: Int = n.signum

    override def unary_-(): Rational = Rational(-SafeLong(n), SafeLong(d))

    def +(r: Rational): Rational = r match {
      case r: LongRational => r + this
      case r: BigRationals.BigRational =>
        val dgcd: BigInt = d.gcd(r.d)
        if (dgcd == 1) {
          Rational(SafeLong(r.d * n + r.n * d), SafeLong(r.d * d))
        } else {
          val lden: BigInt = d / dgcd
          val rden: BigInt = r.d / dgcd
          val num: BigInt = rden * n + r.n * lden
          val ngcd: BigInt = num.gcd(dgcd)
          if (ngcd == 1)
            Rational(SafeLong(num), SafeLong(lden * r.d))
          else
            Rational(SafeLong(num / ngcd), SafeLong(r.d / ngcd) * lden)
        }
    }


    def -(r: Rational): Rational = r match {
      case r: LongRational => (-r) + this
      case r: BigRationals.BigRational =>
        val dgcd: BigInt = d.gcd(r.d)
        if (dgcd == 1) {
          Rational(SafeLong(r.d * n - r.n * d), SafeLong(r.d * d))
        } else {
          val lden: BigInt = d / dgcd
          val rden: BigInt = r.d / dgcd
          val num: BigInt = rden * n - r.n * lden
          val ngcd: BigInt = num.gcd(dgcd)
          if (ngcd == 1)
            Rational(SafeLong(num), SafeLong(lden * r.d))
          else
            Rational(SafeLong(num / ngcd), SafeLong(r.d / ngcd) * lden)
        }
    }


    def *(r: Rational): Rational = r match {
      case r: LongRational => r * this
      case r: BigRationals.BigRational =>
        val a = n.gcd(r.d)
        val b = d.gcd(r.n)
        Rational(SafeLong((n / a) * (r.n / b)), SafeLong((d / b) * (r.d / a)))
    }


    def /(r: Rational): Rational = r match {
      case r: LongRational => r.inverse * this
      case r: BigRationals.BigRational =>
        val a = n.gcd(r.n)
        val b = d.gcd(r.d)
        val num = SafeLong(n / a) * (r.d / b)
        val den = SafeLong(d / b) * (r.n / a)
        if (den < SafeLong.zero) Rational(-num, -den) else Rational(num, den)
    }

    def gcd(r: Rational): Rational = r match {
      case r: LongRational => r gcd this
      case r: BigRationals.BigRational =>
        val dgcd: BigInt = d.gcd(r.d)
        if (dgcd == 1) {
          Rational(n.abs gcd r.n.abs, d * r.d)
        } else {
          val lm = d / dgcd
          val rm = r.d / dgcd
          Rational((n * rm).abs gcd (r.n * lm).abs, dgcd * lm * rm)
        }
    }

    def floor: Rational =
      if (d == 1) this
      else if (n >= 0) Rational(n / d, BigInt(1))
      else Rational(n / d - 1, BigInt(1))

    def ceil: Rational =
      if (d == 1) this
      else if (n >= 0) Rational(n / d + 1, BigInt(1))
      else Rational(n / d, BigInt(1))

    def round: Rational =
      if (n >= 0) {
        val m = (n % d)
        if (m >= (d - m)) Rational(n / d + 1) else Rational(n / d)
      } else {
        val m = -(n % d)
        if (m >= (d - m)) Rational(n / d - 1) else Rational(n / d)
      }

    def pow(exp: Int): Rational = if (exp == 0)
      Rational.one
    else if (exp < 0)
      BigRationals.build(d pow -exp, n pow -exp)
    else
      BigRationals.build(n pow exp, d pow exp)

    def compareToOne: Int = n compare d

    def compare(r: Rational): Int = r match {
      case r: LongRational => {
        val dgcd = spire.math.gcd(r.d, (d % r.d).toLong)
        if (dgcd == 1L)
          (SafeLong(n) * r.d - SafeLong(r.n) * d).signum
        else
          (SafeLong(n) * (r.d / dgcd) - SafeLong(r.n) * (d / dgcd)).signum
      }
      case r: BigRationals.BigRational => {
        val dgcd = d.gcd(r.d)
        if (dgcd == 1)
          (SafeLong(n * r.d) - r.n * d).signum
        else
          (SafeLong(r.d / dgcd) * n - SafeLong(d / dgcd) * r.n).signum
      }
    }
  }
}

trait RationalInstances {
  implicit final val RationalAlgebra = new RationalAlgebra
  implicit def RationalIsNRoot(implicit c:ApproximationContext[Rational]) = new RationalIsNRoot0
}

private trait RationalIsField extends Field[Rational] {
  override def minus(a:Rational, b:Rational): Rational = a - b
  def negate(a:Rational): Rational = -a
  def one: Rational = Rational.one
  def plus(a:Rational, b:Rational): Rational = a + b
  override def pow(a:Rational, b:Int): Rational = a.pow(b)
  override def times(a:Rational, b:Rational): Rational = a * b
  def zero: Rational = Rational.zero
  def quot(a:Rational, b:Rational) = a /~ b
  def mod(a:Rational, b:Rational) = a % b
  override def quotmod(a:Rational, b:Rational) = a /% b
  def gcd(a:Rational, b:Rational):Rational = a gcd b
  override def fromInt(n: Int): Rational = Rational(n)
  override def fromDouble(n: Double): Rational = Rational(n)
  def div(a:Rational, b:Rational) = a / b
}

private trait RationalIsNRoot extends NRoot[Rational] with Serializable {
  implicit def context:ApproximationContext[Rational]
  def nroot(a: Rational, k: Int): Rational = a.nroot(k)
  def fpow(a: Rational, b: Rational): Rational = a.pow(b)
}

private trait RationalIsReal extends IsReal[Rational] {
  override def eqv(x:Rational, y:Rational) = x == y
  override def neqv(x:Rational, y:Rational) = x != y
  override def gt(x: Rational, y: Rational) = x > y
  override def gteqv(x: Rational, y: Rational) = x >= y
  override def lt(x: Rational, y: Rational) = x < y
  override def lteqv(x: Rational, y: Rational) = x <= y
  def compare(x: Rational, y: Rational) = if (x < y) -1 else if (x > y) 1 else 0

  override def sign(a: Rational): Sign = a.sign
  def signum(a: Rational): Int = a.signum
  def abs(a: Rational): Rational = a.abs

  def toDouble(r: Rational): Double = r.toDouble
  def ceil(a:Rational): Rational = a.ceil
  def floor(a:Rational): Rational = a.floor
  def round(a:Rational): Rational = a.round
  def isWhole(a:Rational) = a.denominator == 1
}

@SerialVersionUID(0L)
class RationalAlgebra extends RationalIsField with RationalIsReal with Serializable

@SerialVersionUID(0L)
class RationalIsNRoot0(implicit val context: ApproximationContext[Rational])
extends RationalIsNRoot with Serializable


case class ApproximationContext[A](error: A)
object ApproximationContext {
  implicit def rational2error(q: Rational) = ApproximationContext(q)
}
