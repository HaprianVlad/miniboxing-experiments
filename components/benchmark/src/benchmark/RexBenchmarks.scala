package benchmark


import scala.{specialized => spec}



import scala.reflect.ClassTag
// TODO: remove dependencies from spire.math
import spire.math._
import spire.implicits._
import scala.util.Random._
import com.google.caliper.Param

//imports used for Spire Double and Long implementations


import java.lang.Math
import java.lang.Long.{ numberOfTrailingZeros, numberOfLeadingZeros }
import java.lang.Double.{ longBitsToDouble, doubleToLongBits }
import java.lang.Float.{ intBitsToFloat, floatToIntBits }
import scala.annotation.{ switch, tailrec }

// Rex BENCHMARK

object RexBenchmarks extends MyRunner(classOf[RexBenchmarks])

class RexBenchmarks extends MyBenchmark with BenchmarkData {
  @Param(Array("10", "12", "14", "16", "18"))
  var pow: Int = 0

  var fs: Array[Float] = null
  var ds: Array[Double] = null

  override protected def setUp() {
    val size = spire.math.pow(2, pow).toInt
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
    val ai = new Array[Float](max(k, 0) + 1)
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
    val ai = new Array[Double](max(k, 0) + 1)
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

  def nearlyMaxG[@spec A: Numeric: ClassTag](a: Array[A], k: Int, start: Int = 0, end: Int = -1): A = {
    val i0 = if (start >= 0) start else a.length + start
    val i1 = if (end >= 0) end else a.length + end + 1
    val ai = new Array[A](max(k, 0) + 1)
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
  
  
}

//******************************************************************//

// This part consists on isolating the needed spire implementation for this benchamark

//*****************************************************************//

// 1. Numeric

// TODO: remove dependencies from spire.std
import spire.std._
// TODO: remove dependencies from spire.algebra
import spire.algebra._

trait Numeric[@spec(Int,Long,Float,Double) A] extends Ring[A]
with AdditiveAbGroup[A] with MultiplicativeAbGroup[A] with NRoot[A]
with ConvertableFrom[A] with ConvertableTo[A] with IsReal[A]

object Numeric {
 //implicit final val ByteIsNumeric: Numeric[Byte] = new ByteIsNumeric
 //implicit final val ShortIsNumeric: Numeric[Short] = new ShortIsNumeric
 //implicit final val IntIsNumeric: Numeric[Int] = new IntIsNumeric
 //implicit final val LongIsNumeric: Numeric[Long] = new LongIsNumeric*/
  implicit final val FloatIsNumeric: Numeric[Float] = new FloatIsNumeric
  implicit final val DoubleIsNumeric: Numeric[Double] = new DoubleIsNumeric
  //implicit final val BigIntIsNumeric: Numeric[BigInt] = new BigIntIsNumeric
  //implicit final val BigDecimalIsNumeric: Numeric[BigDecimal] = new BigDecimalIsNumeric
  //implicit final val AlgebraicIsNumeric: Numeric[Algebraic] = new AlgebraicIsNumeric
  //implicit final val RealIsNumeric: Numeric[Real] = new RealIsNumeric

  private val defaultApprox = ApproximationContext(Rational(1, 1000000000))

  //implicit def RationalIsNumeric(implicit ctx: ApproximationContext[Rational] = defaultApprox): Numeric[Rational] =
   //new RationalIsNumeric

  implicit def complexIsNumeric[A: Fractional: Trig: IsReal] = new ComplexIsNumeric

  @inline final def apply[A](implicit ev: Numeric[A]):Numeric[A] = ev
}
/*
@SerialVersionUID(0L)
private class ByteIsNumeric extends Numeric[Byte] with ByteIsEuclideanRing with ByteIsNRoot
with ConvertableFromByte with ConvertableToByte with ByteIsReal with Serializable {
  override def fromInt(n: Int): Byte = n.toByte
  override def fromDouble(n: Double): Byte = n.toByte
  override def toDouble(n: Byte): Double = n.toDouble
  def div(a:Byte, b:Byte): Byte = (a / b).toByte
}


@SerialVersionUID(0L)
private class ShortIsNumeric extends Numeric[Short] with ShortIsEuclideanRing with ShortIsNRoot
with ConvertableFromShort with ConvertableToShort with ShortIsReal with Serializable {
  override def fromInt(n: Int): Short = n.toShort
  override def fromDouble(n: Double): Short = n.toShort
  override def toDouble(n: Short): Double = n.toDouble
  def div(a:Short, b:Short): Short = (a / b).toShort
}

@SerialVersionUID(0L)
private class IntIsNumeric extends Numeric[Int] with IntIsEuclideanRing with IntIsNRoot
with ConvertableFromInt with ConvertableToInt with IntIsReal with Serializable {
  override def fromInt(n: Int): Int = n
  override def fromDouble(n: Double): Int = n.toInt
  override def toDouble(n: Int): Double = n.toDouble
  def div(a: Int, b: Int): Int = a / b
}

@SerialVersionUID(0L)
private class LongIsNumeric extends Numeric[Long] with LongIsEuclideanRing with LongIsNRoot
with ConvertableFromLong with ConvertableToLong with LongIsReal with Serializable {
  override def fromInt(n: Int): Long = n
  override def fromDouble(n: Double): Long = n.toLong
  override def toDouble(n: Long): Double = n.toDouble
  def div(a: Long, b: Long): Long = a / b
}

@SerialVersionUID(0L)
private class BigIntIsNumeric extends Numeric[BigInt] with BigIntIsEuclideanRing
with BigIntIsNRoot with ConvertableFromBigInt with ConvertableToBigInt
with BigIntIsReal with Serializable {
  override def fromInt(n: Int): BigInt = BigInt(n)
  override def fromDouble(n: Double): BigInt = BigDecimal(n).toBigInt
  override def toDouble(n: BigInt): Double = n.toDouble
  def div(a: BigInt, b: BigInt): BigInt = a / b
}
*/
@SerialVersionUID(0L)
private class FloatIsNumeric extends Numeric[Float] with FloatIsField
with FloatIsNRoot with ConvertableFromFloat with ConvertableToFloat
with FloatIsReal with Serializable {
  override def fromInt(n: Int): Float = n.toFloat
  override def fromDouble(n: Double): Float = n.toFloat
  override def toDouble(n: Float): Double = n.toDouble
}

@SerialVersionUID(0L)
private class DoubleIsNumeric extends Numeric[Double] with DoubleIsField
with DoubleIsNRoot with ConvertableFromDouble with ConvertableToDouble
with DoubleIsReal with Serializable {
  override def fromInt(n: Int): Double = n.toDouble
  override def fromDouble(n: Double): Double = n
  override def toDouble(n: Double): Double = n.toDouble
}

/*
@SerialVersionUID(0L)
private class BigDecimalIsNumeric extends Numeric[BigDecimal] with BigDecimalIsField
with BigDecimalIsNRoot with ConvertableFromBigDecimal with ConvertableToBigDecimal
with BigDecimalIsReal with Serializable {
  override def fromInt(n: Int): BigDecimal = BigDecimal(n)
  override def fromDouble(n: Double): BigDecimal = BigDecimal(n)
  override def toDouble(n: BigDecimal): Double = n.toDouble
}

@SerialVersionUID(0L)
private class RationalIsNumeric(implicit val context: ApproximationContext[Rational])
extends Numeric[Rational] with RationalIsField with RationalIsNRoot
with ConvertableFromRational with ConvertableToRational
with RationalIsReal with Serializable {
  override def toDouble(n: Rational): Double = n.toDouble
  override def fromInt(n: Int): Rational = Rational(n)
  override def fromDouble(n: Double): Rational = Rational(n)
}

@SerialVersionUID(0L)
private class AlgebraicIsNumeric extends Numeric[Algebraic] with AlgebraicIsField with AlgebraicIsNRoot
with ConvertableFromAlgebraic with ConvertableToAlgebraic with AlgebraicIsReal with Serializable {
  override def fromInt(n: Int): Algebraic = Algebraic(n)
  override def fromDouble(n: Double): Algebraic = Algebraic(n)
  override def toDouble(n: Algebraic): Double = n.toDouble
}

@SerialVersionUID(0L)
private class RealIsNumeric extends Numeric[Real] with RealIsFractional with Serializable {
  override def fromInt(n: Int): Real = Real(n)
  override def fromDouble(n: Double): Real = Real(n)
  override def toDouble(n: Real): Double = n.toDouble
}

@SerialVersionUID(0L)
class ComplexIsNumeric[A](implicit
    val algebra: Fractional[A], val trig: Trig[A], val order: IsReal[A])
extends ComplexEq[A] with ComplexIsField[A] with Numeric[Complex[A]]
with ComplexIsTrig[A] with ComplexIsNRoot[A]
with ConvertableFromComplex[A] with ConvertableToComplex[A]
with Order[Complex[A]] with ComplexIsSigned[A] with Serializable {
  def nroot: NRoot[A] = algebra

  override def fromInt(n: Int): Complex[A] = Complex.fromInt[A](n)
  override def fromDouble(n: Double): Complex[A] = Complex[A](algebra.fromDouble(n))

  override def eqv(x: Complex[A], y: Complex[A]): Boolean = x == y
  override def nroot(a: Complex[A], n: Int) = a.pow(reciprocal(fromInt(n)))

  def compare(x:Complex[A], y:Complex[A]): Int =
    if (x eqv y) 0 else throw new UnsupportedOperationException("undefined")

  def ceil(a: Complex[A]): Complex[A] = a.ceil
  def floor(a: Complex[A]): Complex[A] = a.floor
  def isWhole(a: Complex[A]): Boolean = a.isWhole
  def round(a: Complex[A]): Complex[A] = a.round
}

//Complex for Numeric


object Complex extends ComplexInstances {
  def i[@spec(Float, Double) T](implicit T: Rig[T]) =
    new Complex(T.zero, T.one)

  def one[@spec(Float, Double) T](implicit T: Rig[T]) =
    new Complex(T.one, T.zero)

  def zero[@spec(Float, Double) T](implicit T: Semiring[T]) =
    new Complex(T.zero, T.zero)

  def fromInt[@spec(Float, Double) T](n: Int)(implicit f: Ring[T]) =
    new Complex(f.fromInt(n), f.zero)

  implicit def intToComplex(n: Int) = new Complex(n.toDouble, 0.0)
  implicit def longToComplex(n: Long) = new Complex(n.toDouble, 0.0)
  implicit def floatToComplex(n: Float) = new Complex(n, 0.0F)
  implicit def doubleToComplex(n: Double) = new Complex(n, 0.0)

  implicit def bigIntToComplex(n: BigInt): Complex[BigDecimal] =
    bigDecimalToComplex(BigDecimal(n))

  implicit def bigDecimalToComplex(n: BigDecimal): Complex[BigDecimal] = {
    implicit val mc = n.mc
    new Complex(n, BigDecimal(0))
  }

  def polar[@spec(Float, Double) T: Field: Trig](magnitude: T, angle: T): Complex[T] =
    new Complex(magnitude * Trig[T].cos(angle), magnitude * Trig[T].sin(angle))

  def apply[@spec(Float, Double) T: Semiring](real: T): Complex[T] =
    new Complex(real, Semiring[T].zero)

  def rootOfUnity[@spec(Float, Double) T](n: Int, x: Int)(implicit f: Field[T], t: Trig[T], r: IsReal[T]): Complex[T] = {
    if (x == 0) return one[T]

    if (n % 2 == 0) {
      if (x == n / 2) return -one[T]
      if (n % 4 == 0) {
        if (x == n / 4) return i[T]
        if (x == n * 3 / 4) return -i[T]
      }
    }

    polar(f.one, (t.pi * 2 * x) / n)
  }

  def rootsOfUnity[@spec(Float, Double) T](n: Int)(implicit f: Field[T], t: Trig[T], r: IsReal[T]): Array[Complex[T]] = {
    val roots = new Array[Complex[T]](n)
    var sum = one[T]
    roots(0) = sum

    val west = if (n % 2 == 0) n / 2 else -1
    val north = if (n % 4 == 0) n / 4 else -1
    val south = if (n % 4 == 0) 3 * n / 4 else -1

    var x = 1
    val last = n - 1
    while (x < last) {
      val c = x match {
        case `north` => i[T]
        case `west` => -one[T]
        case `south` => -i[T]
        case _ => polar(f.one, (t.pi * 2 * x) / n)
      }
      roots(x) = c
      sum += c
      x += 1
    }

    roots(last) = zero[T] - sum
    roots
  }
}

@SerialVersionUID(0L)
final case class Complex[@spec(Float, Double) T](real: T, imag: T)
    extends ScalaNumber with ScalaNumericConversions with Serializable { lhs =>

  import spire.syntax.order._

  /**
   * This returns the sign of `real` if it is not 0, otherwise it returns the
   * sign of `imag`.
   */
  def signum(implicit o: IsReal[T]): Int = real.signum match {
    case 0 => imag.signum
    case n => n
  }

  /**
   * This implements sgn(z), which (except for z=0) observes:
   * 
   * `sgn(z) = z / abs(z) = abs(z) / z`
   */
  def complexSignum(implicit f: Field[T], o: IsReal[T], n: NRoot[T]): Complex[T] =
    if (isZero) this else this / abs

  def abs(implicit f: Field[T], o: IsReal[T], n: NRoot[T]): T =
    (real * real + imag * imag).sqrt

  def arg(implicit f: Field[T], t: Trig[T], o: IsReal[T]): T =
    if (isZero) f.zero else t.atan2(imag, real)

  def norm(implicit f: Field[T], n: NRoot[T]): T =
    (real * real + imag * imag).sqrt

  def conjugate(implicit f: Rng[T]): Complex[T] = new Complex(real, -imag)

  def asTuple: (T, T) = (real, imag)
  def asPolarTuple(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): (T, T) = (abs, arg)

  def isZero(implicit o: IsReal[T]): Boolean = real.isZero && imag.isZero
  def isImaginary(implicit o: IsReal[T]): Boolean = real.isZero
  def isReal(implicit o: IsReal[T]): Boolean = imag.isZero

  def eqv(b: Complex[T])(implicit o: Eq[T]): Boolean = real === b.real && imag === b.imag
  def neqv(b: Complex[T])(implicit o: Eq[T]): Boolean = real =!= b.real || imag =!= b.imag

  def unary_-(implicit r: Rng[T]): Complex[T] = new Complex(-real, -imag)

  def +(rhs: T)(implicit r: Semiring[T]): Complex[T] = new Complex(real + rhs, imag)
  def -(rhs: T)(implicit r: Rng[T]): Complex[T] = new Complex(real - rhs, imag)
  def *(rhs: T)(implicit r: Semiring[T]): Complex[T] = new Complex(real * rhs, imag * rhs)
  def /(rhs: T)(implicit r: Field[T]): Complex[T] = new Complex(real / rhs, imag / rhs)

  // TODO: instead of floor should be round-toward-zero

  def /~(rhs: T)(implicit f: Field[T], o: IsReal[T]): Complex[T] = (this / rhs).floor
  def %(rhs: T)(implicit f: Field[T], o: IsReal[T]): Complex[T] = this - (this /~ rhs) * rhs
  def /%(rhs: T)(implicit f: Field[T], o: IsReal[T]): (Complex[T], Complex[T]) = {
    val q = this /~ rhs
    (q, this - q * rhs)
  }

  def **(e: T)(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] = this pow e
  def pow(e: T)(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] =
    if (e.isZero) {
      Complex.one[T]
    } else if (this.isZero) {
      if (e < f.zero)
        throw new Exception("raising 0 to negative/complex power")
      Complex.zero[T]
    } else {
      Complex.polar(abs fpow e, arg * e)
    }

  def +(b: Complex[T])(implicit r: Semiring[T]): Complex[T] =
    new Complex(real + b.real, imag + b.imag)

  def -(b: Complex[T])(implicit r: Rng[T]): Complex[T] =
    new Complex(real - b.real, imag - b.imag)

  def *(b: Complex[T])(implicit r: Rng[T]): Complex[T] =
    new Complex(real * b.real - imag * b.imag, imag * b.real + real * b.imag)

  def /(b: Complex[T])(implicit f: Field[T], o: IsReal[T]): Complex[T] = {
    val abs_breal = b.real.abs
    val abs_bimag = b.imag.abs

    if (abs_breal >= abs_bimag) {
      if (abs_breal === f.zero) throw new Exception("/ by zero")
      val ratio = b.imag / b.real
      val denom = b.real + b.imag * ratio
      new Complex((real + imag * ratio) / denom, (imag - real * ratio) / denom)

    } else {
      if (abs_bimag === f.zero) throw new Exception("/ by zero")
      val ratio = b.real / b.imag
      val denom = b.real * ratio + b.imag
      new Complex((real * ratio + imag) / denom, (imag * ratio - real) /denom)
    }
  }

  def /~(b: Complex[T])(implicit f: Field[T], o: IsReal[T]): Complex[T] = {
    val d = this / b
    new Complex(d.real.floor, d.imag.floor)
  }

  def %(b: Complex[T])(implicit f: Field[T], o: IsReal[T]): Complex[T] = this - (this /~ b) * b

  def /%(b: Complex[T])(implicit f: Field[T], o: IsReal[T]): (Complex[T], Complex[T]) = {
    val q = this /~ b
    (q, this - q * b)
  }

  def **(b: Int)(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] = pow(b)

  def nroot(k: Int)(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] =
    if (isZero) Complex.zero else pow(Complex(f.fromInt(k).reciprocal, f.zero))

  def pow(b: Int)(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] =
    if (isZero) Complex.zero else Complex.polar(abs.pow(b), arg * b)

  def **(b: Complex[T])(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] = pow(b)

  def pow(b: Complex[T])(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] =
    if (b.isZero) {
      Complex.one[T]
    } else if (this.isZero) {
      if (b.imag =!= f.zero || b.real < f.zero)
        throw new Exception("raising 0 to negative/complex power")
      Complex.zero[T]
    } else if (b.imag =!= f.zero) {
      val len = (abs fpow b.real) / t.exp(arg * b.imag)
      val phase = arg * b.real + t.log(abs) * b.imag
      Complex.polar(len, phase)
    } else {
      Complex.polar(abs fpow b.real, arg * b.real)
    }

  // we are going with the "principal value" definition of Log.
  def log(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] = {
    if (isZero) throw new IllegalArgumentException("log(0) undefined")
    new Complex(t.log(abs), arg)
  }

  def sqrt(implicit f: Field[T], n0: NRoot[T], o: IsReal[T]): Complex[T] = {
    if (isZero) {
      Complex.zero[T]
    } else {
      val two = f.fromInt(2)
      val a = ((abs + real.abs) / two).sqrt
      imag.signum match {
        case 0 =>
          if (real < f.zero) Complex(f.zero, a) else Complex(a, f.zero)
        case n =>
          val b = ((abs - real.abs) / two).sqrt
          if (n < 0) Complex(a, -b) else Complex(a, b)
      }
    }
  }

  def floor(implicit o: IsReal[T]): Complex[T] = new Complex(real.floor, imag.floor)
  def ceil(implicit o: IsReal[T]): Complex[T] = new Complex(real.ceil, imag.ceil)
  def round(implicit o: IsReal[T]): Complex[T] = new Complex(real.round, imag.round)

  // acos(z) = -i*(log(z + i*(sqrt(1 - z*z))))
  def acos(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] = {
    val z2 = this * this
    val s = new Complex(f.one - z2.real, -z2.imag).sqrt
    val l = new Complex(real + s.imag, imag + s.real).log
    new Complex(l.imag, -l.real)
  }

  // asin(z) = -i*(log(sqrt(1 - z*z) + i*z))
  def asin(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] = {
    val z2 = this * this
    val s = new Complex(f.one - z2.real, -z2.imag).sqrt
    val l = new Complex(s.real + -imag, s.imag + real).log
    new Complex(l.imag, -l.real)
  }

  // atan(z) = (i/2) log((i + z)/(i - z))
  def atan(implicit f: Field[T], r: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] = {
    val n = new Complex(real, imag + f.one)
    val d = new Complex(-real, f.one - imag)
    val l = (n / d).log
    new Complex(l.imag / f.fromInt(-2), l.real / f.fromInt(2))
  }

  // exp(a+ci) = (exp(a) * cos(c)) + (exp(a) * sin(c))i
  def exp(implicit f: Field[T], t: Trig[T]): Complex[T] =
    new Complex(t.exp(real) * t.cos(imag), t.exp(real) * t.sin(imag))

  // sin(a+ci) = (sin(a) * cosh(c)) + (cos(a) * sinh(c))i
  def sin(implicit f: Field[T], t: Trig[T]): Complex[T] =
    new Complex(t.sin(real) * t.cosh(imag), t.cos(real) * t.sinh(imag))

  // sinh(a+ci) = (sinh(a) * cos(c)) + (cosh(a) * sin(c))i
  def sinh(implicit f: Field[T], t: Trig[T]): Complex[T] =
    new Complex(t.sinh(real) * t.cos(imag), t.cosh(real) * t.sin(imag))

  // cos(a+ci) = (cos(a) * cosh(c)) - (sin(a) * sinh(c))i 
  def cos(implicit f: Field[T], t: Trig[T]): Complex[T] =
    new Complex(t.cos(real) * t.cosh(imag), -t.sin(real) * t.sinh(imag))

  // cosh(a+ci) = (cosh(a) * cos(c)) + (sinh(a) * sin(c))i 
  def cosh(implicit f: Field[T], t: Trig[T]): Complex[T] =
    new Complex(t.cosh(real) * t.cos(imag), t.sinh(real) * t.sin(imag))

  // tan(a+ci) = (sin(a+a) + sinh(c+c)i) / (cos(a+a) + cosh(c+c))
  def tan(implicit f: Field[T], t: Trig[T]): Complex[T] = {
    val r2 = real + real
    val i2 = imag + imag
    val d = t.cos(r2) + t.cosh(i2)
    new Complex(t.sin(r2) / d, t.sinh(i2) / d)
  }

  // tanh(a+ci) = (sinh(a+a) + sin(c+c)i) / (cosh(a+a) + cos(c+c))
  def tanh(implicit f: Field[T], t: Trig[T]): Complex[T] = {
    val r2 = real + real
    val i2 = imag + imag
    val d = t.cos(r2) + t.cosh(i2)
    new Complex(t.sinh(r2) / d, t.sin(i2) / d)
  }

  // junky ScalaNumber stuff
  def floatValue: Float = doubleValue.toFloat
  def doubleValue: Double = anyToDouble(real)
  override def byteValue: Byte = longValue.toByte
  override def shortValue: Short = longValue.toShort
  def intValue: Int = longValue.toInt
  override def longValue: Long = anyToLong(real)

  def underlying: Object = this

  def isWhole: Boolean =
    anyIsZero(imag) && anyIsWhole(real)

  override final def isValidInt: Boolean =
    anyIsZero(imag) && anyIsValidInt(real)

  // important to keep in sync with Quaternion[_]
  override def hashCode: Int =
    if (anyIsZero(imag)) real.##
    else (19 * real.##) + (41 * imag.##) + 97

  // not typesafe, so this is the best we can do :(
  override def equals(that: Any): Boolean = that match {
    case that: Complex[_] =>
      real == that.real && imag == that.imag
    case that: Quaternion[_] =>
      real == that.r && imag == that.i && anyIsZero(that.j) && anyIsZero(that.k)
    case that =>
      anyIsZero(imag) && real == that
  }

  override def toString: String = s"($real + ${imag}i)"

  def toQuaternion(implicit ev: AdditiveMonoid[T]): Quaternion[T] =
    Quaternion(real, imag, ev.zero, ev.zero)
}


object FloatComplex {
  import FastComplex.{encode}

  final def apply(real: Float, imag: Float): FloatComplex =
    new FloatComplex(encode(real, imag))

  final def apply(real: Double, imag: Double) =
    new FloatComplex(encode(real.toFloat, imag.toFloat))

  def polar(magnitude: Float, angle: Float) =
    new FloatComplex(FastComplex.polar(magnitude, angle))

  final val i = new FloatComplex(4575657221408423936L)
  final val one = new FloatComplex(1065353216L)
  final val zero = new FloatComplex(0L)
}

/**
 * Value class which encodes two floating point values in a Long.
 *
 * We get (basically) unboxed complex numbers using this hack.
 * The underlying implementation lives in the FastComplex object.
 */
class FloatComplex(val u: Long) extends AnyVal {
  override final def toString: String = "(%s+%si)" format (real, imag)

  final def real: Float = FastComplex.real(u)
  final def imag: Float = FastComplex.imag(u)
  final def repr = "FloatComplex(%s, %s)" format(real, imag)
  final def abs: Float = FastComplex.abs(u)
  final def angle: Float = FastComplex.angle(u)
  final def conjugate = new FloatComplex(FastComplex.conjugate(u))
  final def isWhole: Boolean = FastComplex.isWhole(u)
  final def signum: Int = FastComplex.signum(u)
  final def complexSignum = new FloatComplex(FastComplex.complexSignum(u))
  final def negate = new FloatComplex(FastComplex.negate(u))

  final def +(b: FloatComplex) = new FloatComplex(FastComplex.add(u, b.u))
  final def -(b: FloatComplex) = new FloatComplex(FastComplex.subtract(u, b.u))
  final def *(b: FloatComplex) = new FloatComplex(FastComplex.multiply(u, b.u))
  final def /(b: FloatComplex) = new FloatComplex(FastComplex.divide(u, b.u))
  final def /~(b: FloatComplex) = new FloatComplex(FastComplex.quot(u, b.u))
  final def %(b: FloatComplex) = new FloatComplex(FastComplex.mod(u, b.u))

  final def /%(b: FloatComplex) = FastComplex.quotmod(u, b.u) match {
    case (q, m) => (new FloatComplex(q), new FloatComplex(m))
  }

  final def pow(b: FloatComplex) = new FloatComplex(FastComplex.pow(u, b.u))
  final def **(b: FloatComplex) = pow(b)

  final def pow(b: Int) = new FloatComplex(FastComplex.pow(u, FastComplex(b.toFloat, 0.0F)))
  final def **(b: Int) = pow(b)
}


/**
 * FastComplex is an ugly, beautiful hack.
 *
 * The basic idea is to encode two 32-bit Floats into a single 64-bit Long.
 * The lower-32 bits are the "real" Float and the upper-32 are the "imaginary"
 * Float.
 *
 * Since we're overloading the meaning of Long, all the operations have to be
 * defined on the FastComplex object, meaning the syntax for using this is a
 * bit ugly. To add to the ugly beauty of the whole thing I could imagine
 * defining implicit operators on Long like +@, -@, *@, /@, etc.
 *
 * You might wonder why it's even worth doing this. The answer is that when
 * you need to allocate an array of e.g. 10-20 million complex numbers, the GC
 * overhead of using *any* object is HUGE. Since we can't build our own
 * "pass-by-value" types on the JVM we are stuck doing an encoding like this.
 *
 * Here are some profiling numbers for summing an array of complex numbers,
 * timed against a concrete case class implementation using Float (in ms):
 *
 *  size | encoded |  class
 *    1M |     5.1 |    5.8
 *    5M |    28.5 |   91.7
 *   10M |    67.7 |  828.1
 *   20M |   228.0 | 2687.0
 *
 * Not bad, eh?
 */
object FastComplex {
  import java.lang.Math.{atan2, cos, sin, sqrt}

  // note the superstitious use of @inline and final everywhere

  final def apply(real: Float, imag: Float) = encode(real, imag)
  final def apply(real: Double, imag: Double) = encode(real.toFloat, imag.toFloat)

  // encode a float as some bits
  @inline final def bits(n: Float): Int = java.lang.Float.floatToRawIntBits(n)

  // decode some bits into a float
  @inline final def bits(n: Int): Float = java.lang.Float.intBitsToFloat(n)

  // get the real part of the complex number
  @inline final def real(d: Long): Float = bits((d & 0xffffffff).toInt)

  // get the imaginary part of the complex number
  @inline final def imag(d: Long): Float = bits((d >> 32).toInt)

  // define some handy constants
  final val i = encode(0.0F, 1.0F)
  final val one = encode(1.0F, 0.0F)
  final val zero = encode(0.0F, 0.0F)

  // encode two floats representing a complex number
  @inline final def encode(real: Float, imag: Float): Long = {
    (bits(imag).toLong << 32) + bits(real).toLong
  }

  // encode two floats representing a complex number in polar form
  @inline final def polar(magnitude: Float, angle: Float): Long = {
    encode(magnitude * cos(angle).toFloat, magnitude * sin(angle).toFloat)
  }

  // decode should be avoided in fast code because it allocates a Tuple2.
  final def decode(d: Long): (Float, Float) = (real(d), imag(d))

  // produces a string representation of the Long/(Float,Float)
  final def toRepr(d: Long): String = "FastComplex(%s -> %s)" format(d, decode(d))

  // get the magnitude/absolute value
  final def abs(d: Long): Float = {
    val re = real(d)
    val im = imag(d)
    java.lang.Math.sqrt(re * re + im * im).toFloat
  }

  // get the angle/argument
  final def angle(d: Long): Float = atan2(imag(d), real(d)).toFloat

  // get the complex conjugate
  final def conjugate(d: Long): Long = encode(real(d), -imag(d))

  // see if the complex number is a whole value
  final def isWhole(d: Long): Boolean = real(d) % 1.0F == 0.0F && imag(d) % 1.0F == 0.0F

  // get the sign of the complex number
  final def signum(d: Long): Int = real(d) compare 0.0F

  // get the complex sign of the complex number
  final def complexSignum(d: Long): Long = {
    val m = abs(d)
    if (m == 0.0F) zero else divide(d, encode(m, 0.0F))
  }

  // negation
  final def negate(a: Long): Long = encode(-real(a), -imag(a))

  // addition
  final def add(a: Long, b: Long): Long = encode(real(a) + real(b), imag(a) + imag(b))

  // subtraction
  final def subtract(a: Long, b: Long): Long = encode(real(a) - real(b), imag(a) - imag(b))

  // multiplication
  final def multiply(a: Long, b: Long): Long = {
    val re_a = real(a)
    val im_a = imag(a)
    val re_b = real(b)
    val im_b = imag(b)
    encode(re_a * re_b - im_a * im_b, im_a * re_b + re_a * im_b)
  }

  // division
  final def divide(a: Long, b: Long): Long = {
    val re_a = real(a)
    val im_a = imag(a)
    val re_b = real(b)
    val im_b = imag(b)

    val abs_re_b = Math.abs(re_b)
    val abs_im_b = Math.abs(im_b)

    if (abs_re_b >= abs_im_b) {
      if (abs_re_b == 0.0F) throw new ArithmeticException("/0")
      val ratio = im_b / re_b
      val denom = re_b + im_b * ratio
      encode((re_a + im_a * ratio) / denom, (im_a - re_a * ratio) / denom)

    } else {
      if (abs_im_b == 0.0F) throw new ArithmeticException("/0")
      val ratio = re_b / im_b
      val denom = re_b * ratio + im_b
      encode((re_a * ratio + im_a) / denom, (im_a * ratio - re_a) / denom)
    }
  }

  final def quot(a: Long, b: Long): Long =
    encode(Math.floor(real(divide(a, b))).toFloat, 0.0F)

  final def mod(a: Long, b: Long): Long = subtract(a, multiply(b, quot(a, b)))

  final def quotmod(a: Long, b: Long): (Long, Long) = {
    val q = quot(a, b)
    (q, subtract(a, multiply(b, quot(a, b))))
  }

  // exponentiation
  final def pow(a: Long, b: Long): Long = if (b == zero) {
    encode(1.0F, 0.0F)

  } else if (a == zero) {
    if (imag(b) != 0.0F || real(b) < 0.0F)
      throw new Exception("raising 0 to negative/complex power")
    zero

  } else if (imag(b) != 0.0F) {
    val im_b = imag(b)
    val re_b = real(b)
    val len = (Math.pow(abs(a), re_b) / exp((angle(a) * im_b))).toFloat
    val phase = (angle(a) * re_b + log(abs(a)) * im_b).toFloat
    polar(len, phase)

  } else {
    val len = Math.pow(abs(a), real(b)).toFloat
    val phase = (angle(a) * real(b)).toFloat
    polar(len, phase)
  }
}

trait ComplexInstances0 {
  implicit def ComplexRing[A: Ring: IsReal]: Ring[Complex[A]] = new ComplexIsRingImpl[A]
}

trait ComplexInstances1 extends ComplexInstances0 {
  implicit def ComplexField[A: Field: IsReal]: Field[Complex[A]] = new ComplexIsFieldImpl[A]
}

trait ComplexInstances extends ComplexInstances1 {
  implicit def ComplexAlgebra[@spec(Float, Double) A: Fractional: Trig: IsReal] =
    new ComplexAlgebra[A]

  implicit def ComplexEq[A: Eq]: Eq[Complex[A]] = new ComplexEq[A]
}

private trait ComplexIsRing[@spec(Float, Double) A] extends Ring[Complex[A]] {
  implicit def algebra: Ring[A]
  implicit def order: IsReal[A]

  override def minus(a: Complex[A], b: Complex[A]): Complex[A] = a - b
  def negate(a: Complex[A]): Complex[A] = -a
  def one: Complex[A] = Complex.one
  def plus(a: Complex[A], b: Complex[A]): Complex[A] = a + b
  override def times(a: Complex[A], b: Complex[A]): Complex[A] = a * b
  def zero: Complex[A] = Complex.zero

  override def fromInt(n: Int): Complex[A] = Complex.fromInt[A](n)
}

private trait ComplexIsField[@spec(Float,Double) A]
extends ComplexIsRing[A] with Field[Complex[A]] {
  import spire.syntax.order._

  implicit def algebra: Field[A]

  override def fromDouble(n: Double): Complex[A] = Complex(algebra.fromDouble(n))
  def div(a: Complex[A], b: Complex[A]) = a / b
  def quot(a: Complex[A], b: Complex[A]) = a /~ b
  def mod(a: Complex[A], b: Complex[A]) = a % b
  override def quotmod(a: Complex[A], b: Complex[A]) = a /% b
  def gcd(a: Complex[A], b: Complex[A]): Complex[A] = {
    @tailrec def _gcd(a: Complex[A], b: Complex[A]): Complex[A] =
      if (b.isZero) a else _gcd(b, a - (a / b).round * b)
    _gcd(a, b)
  }
}

private trait ComplexIsTrig[@spec(Float, Double) A] extends Trig[Complex[A]] {
  implicit def algebra: Field[A]
  implicit def nroot: NRoot[A]
  implicit def trig: Trig[A]
  implicit def order: IsReal[A]

  def e: Complex[A] = new Complex[A](trig.e, algebra.zero)
  def pi: Complex[A] = new Complex[A](trig.pi, algebra.zero)

  def exp(a: Complex[A]): Complex[A] = a.exp
  def expm1(a: Complex[A]): Complex[A] = a.exp - algebra.one
  def log(a: Complex[A]): Complex[A] = a.log
  def log1p(a: Complex[A]): Complex[A] = (a + algebra.one).log

  def sin(a: Complex[A]): Complex[A] = a.sin
  def cos(a: Complex[A]): Complex[A] = a.cos
  def tan(a: Complex[A]): Complex[A] = a.tan

  def asin(a: Complex[A]): Complex[A] = a.sin
  def acos(a: Complex[A]): Complex[A] = a.cos
  def atan(a: Complex[A]): Complex[A] = a.tan
  def atan2(y: Complex[A], x: Complex[A]): Complex[A] =
    new Complex(x.real, y.imag).atan

  def sinh(x: Complex[A]): Complex[A] = x.sinh
  def cosh(x: Complex[A]): Complex[A] = x.cosh
  def tanh(x: Complex[A]): Complex[A] = x.tanh

  def toRadians(a: Complex[A]): Complex[A] = a
  def toDegrees(a: Complex[A]): Complex[A] = a
}

private trait ComplexIsNRoot[A] extends NRoot[Complex[A]] {
  implicit def algebra: Field[A]
  implicit def nroot: NRoot[A]
  implicit def trig: Trig[A]
  implicit def order: IsReal[A]

  def nroot(a: Complex[A], k: Int): Complex[A] = a.nroot(k)
  override def sqrt(a: Complex[A]): Complex[A] = a.sqrt
  def fpow(a: Complex[A], b: Complex[A]): Complex[A] = a.pow(b)
}

private trait ComplexIsSigned[A] extends Signed[Complex[A]] {
  implicit def algebra: Field[A]
  implicit def nroot: NRoot[A]
  implicit def order: IsReal[A]

  def signum(a: Complex[A]): Int = a.signum
  def abs(a: Complex[A]): Complex[A] = Complex[A](a.abs, algebra.zero)
}

@SerialVersionUID(1L)
private class ComplexEq[A: Eq] extends Eq[Complex[A]] with Serializable {
  def eqv(x: Complex[A], y: Complex[A]) = x eqv y
  override def neqv(x: Complex[A], y: Complex[A]) = x neqv y
}

@SerialVersionUID(1L)
private final class ComplexIsRingImpl[@spec(Float,Double) A](implicit
    val algebra: Ring[A], val order: IsReal[A]) extends ComplexIsRing[A] with Serializable

@SerialVersionUID(1L)
private final class ComplexIsFieldImpl[@spec(Float,Double) A](implicit
    val algebra: Field[A], val order: IsReal[A]) extends ComplexIsField[A] with Serializable

@SerialVersionUID(1L)
private class ComplexAlgebra[@spec(Float, Double) A](implicit
      val algebra: Field[A], val nroot: NRoot[A], val trig: Trig[A], val order: IsReal[A])
    extends ComplexIsField[A]
    with ComplexIsTrig[A]
    with ComplexIsNRoot[A]
    with ComplexIsSigned[A]
    with InnerProductSpace[Complex[A], A]
    with FieldAlgebra[Complex[A], A]
    with Serializable {
  def scalar = algebra
  def timesl(a: A, v: Complex[A]): Complex[A] = Complex(a, scalar.zero) * v
  def dot(x: Complex[A], y: Complex[A]): A =
    scalar.plus(scalar.times(x.real, y.real), scalar.times(x.imag, y.imag))
  override def pow(a: Complex[A], b: Int): Complex[A] = a.pow(b)
}
*/

///////////////////////////////////////////////////////////////////////////

//Double implementation

trait DoubleIsField extends Field[Double] {
  override def minus(a:Double, b:Double): Double = a - b
  def negate(a:Double): Double = -a
  def one: Double = 1.0
  def plus(a:Double, b:Double): Double = a + b
 //TODO: take a look at this override and pow method in Rig
  override def pow(a:Double, b:Int): Double = Math.pow(a, b)
  override def times(a:Double, b:Double): Double = a * b
  def zero: Double = 0.0

  override def fromInt(n: Int): Double = n

  def quot(a:Double, b:Double) = (a - (a % b)) / b
  def mod(a:Double, b:Double) = a % b

  final def gcd(a:Double, b:Double):Double = {
    def value(bits: Long): Long = bits & 0x000FFFFFFFFFFFFFL | 0x0010000000000000L

    def exp(bits: Long): Int = ((bits >> 52) & 0x7FF).toInt

    def gcd0(val0: Long, exp0: Int, val1: Long, exp1: Int): Double = {
      val tz0 = numberOfTrailingZeros(val0)
      val tz1 = numberOfTrailingZeros(val1)
      val tzShared = spire.math.min(tz0, tz1 + exp1 - exp0)
      val n = spire.math.gcd(val0 >>> tz0, val1 >>> tz1) << tzShared

      val shift = numberOfLeadingZeros(n) - 11 // Number of bits to move 1 to bit 52
      val mantissa = (n << shift) & 0x000FFFFFFFFFFFFFL
      val exp = (exp0 - shift).toLong
      if (exp < 0) 0.0 else longBitsToDouble((exp << 52) | mantissa)
    }

    if (a == 0D) b
    else if (b == 0D) a
    else {
      val aBits = doubleToLongBits(a)
      val aVal = value(aBits)
      val aExp = exp(aBits)

      val bBits = doubleToLongBits(b)
      val bVal = value(bBits)
      val bExp = exp(bBits)

      if (aExp < bExp) gcd0(aVal, aExp, bVal, bExp)
      else gcd0(bVal, bExp, aVal, aExp)
    }
  }

  override def fromDouble(n: Double): Double = n
  def div(a:Double, b:Double) = a / b
}

trait DoubleIsNRoot extends NRoot[Double] {
  def nroot(a: Double, k: Int): Double = Math.pow(a, 1 / k.toDouble)
  override def sqrt(a: Double): Double = Math.sqrt(a)
  def fpow(a: Double, b: Double) = Math.pow(a, b)
}

trait DoubleIsTrig extends Trig[Double] {
  def e: Double = Math.E
  def pi: Double = Math.PI

  def exp(a: Double): Double = Math.exp(a)
  def expm1(a: Double): Double = Math.expm1(a)
  def log(a: Double) = Math.log(a)
  def log1p(a: Double) = Math.log1p(a)

  def sin(a: Double): Double = Math.sin(a)
  def cos(a: Double): Double = Math.cos(a)
  def tan(a: Double): Double = Math.tan(a)

  def asin(a: Double): Double = Math.asin(a)
  def acos(a: Double): Double = Math.acos(a)
  def atan(a: Double): Double = Math.atan(a)
  def atan2(y: Double, x: Double): Double = Math.atan2(y, x)

  def sinh(x: Double): Double = Math.sinh(x)
  def cosh(x: Double): Double = Math.cosh(x)
  def tanh(x: Double): Double = Math.tanh(x)

  def toRadians(a: Double): Double = (a * 2 * pi) / 360
  def toDegrees(a: Double): Double = (a * 360) / (2 * pi)
}

trait DoubleOrder extends Order[Double] {
  override def eqv(x:Double, y:Double) = x == y
  override def neqv(x:Double, y:Double) = x != y
  override def gt(x: Double, y: Double) = x > y
  override def gteqv(x: Double, y: Double) = x >= y
  override def lt(x: Double, y: Double) = x < y
  override def lteqv(x: Double, y: Double) = x <= y
  override def min(x: Double, y: Double) = Math.min(x, y)
  override def max(x: Double, y: Double) = Math.max(x, y)
  def compare(x: Double, y: Double) = java.lang.Double.compare(x, y)
}

trait DoubleIsSigned extends Signed[Double] {
  def signum(a: Double): Int = Math.signum(a).toInt
  def abs(a: Double): Double = if (a < 0.0) -a else a
}

trait DoubleIsReal extends IsReal[Double] with DoubleOrder with DoubleIsSigned {
  def toDouble(x: Double): Double = x
  def ceil(a:Double): Double = Math.floor(a)
  def floor(a:Double): Double = Math.floor(a)
  def round(a:Double): Double = spire.math.round(a)
  def isWhole(a:Double) = a % 1.0 == 0.0
}

@SerialVersionUID(0L)
class DoubleAlgebra extends DoubleIsField with DoubleIsNRoot with DoubleIsTrig with DoubleIsReal with Serializable

trait DoubleInstances {
  implicit final val DoubleAlgebra = new DoubleAlgebra
}

////////////////////////////////////////////////////////////////////////////////

// Long implementation

trait FloatIsField extends Field[Float] {
  override def minus(a:Float, b:Float): Float = a - b
  def negate(a:Float): Float = -a
  def one: Float = 1.0F
  def plus(a:Float, b:Float): Float = a + b
  override def pow(a:Float, b:Int): Float = Math.pow(a, b).toFloat
  override def times(a:Float, b:Float): Float = a * b
  def zero: Float = 0.0F
  
  override def fromInt(n: Int): Float = n

  def quot(a:Float, b:Float) = (a - (a % b)) / b
  def mod(a:Float, b:Float) = a % b

  final def gcd(a:Float, b:Float):Float = {
    def value(bits: Int): Int = bits & 0x007FFFFF | 0x00800000

    def exp(bits: Int): Int = ((bits >> 23) & 0xFF).toInt

    def gcd0(val0: Int, exp0: Int, val1: Int, exp1: Int): Float = {
      val tz0 = numberOfTrailingZeros(val0)
      val tz1 = numberOfTrailingZeros(val1)
      val tzShared = spire.math.min(tz0, tz1 + exp1 - exp0)
      val n = spire.math.gcd(val0 >>> tz0, val1 >>> tz1).toInt << tzShared

      val shift = numberOfLeadingZeros(n) - 8 // Number of bits to move 1 to bit 23
      val mantissa = (n << shift) & 0x007FFFFF
      val exp = (exp0 - shift)
      if (exp < 0) 0F else intBitsToFloat((exp << 23) | mantissa)
    }

    if (a == 0F) b
    else if (b == 0F) a
    else {
      val aBits = floatToIntBits(a)
      val aVal = value(aBits)
      val aExp = exp(aBits)

      val bBits = floatToIntBits(b)
      val bVal = value(bBits)
      val bExp = exp(bBits)

      if (aExp < bExp) gcd0(aVal, aExp, bVal, bExp)
      else gcd0(bVal, bExp, aVal, aExp)
    }
  }

  override def fromDouble(n: Double): Float = n.toFloat

  def div(a:Float, b:Float) = a / b
}

trait FloatIsNRoot extends NRoot[Float] {
  def nroot(a: Float, k: Int): Float = Math.pow(a, 1 / k.toDouble).toFloat
  override def sqrt(a: Float): Float = Math.sqrt(a).toFloat
  def fpow(a: Float, b: Float) = Math.pow(a, b).toFloat
}

trait FloatIsTrig extends Trig[Float] {
  def e: Float = Math.E.toFloat
  def pi: Float = Math.PI.toFloat

  def exp(a: Float): Float = Math.exp(a).toFloat
  def expm1(a: Float): Float = Math.expm1(a).toFloat
  def log(a: Float) = Math.log(a).toFloat
  def log1p(a: Float) = Math.log1p(a).toFloat

  def sin(a: Float): Float = Math.sin(a.toDouble).toFloat
  def cos(a: Float): Float = Math.cos(a.toDouble).toFloat
  def tan(a: Float): Float = Math.tan(a.toDouble).toFloat

  def asin(a: Float): Float = Math.asin(a.toDouble).toFloat
  def acos(a: Float): Float = Math.acos(a.toDouble).toFloat
  def atan(a: Float): Float = Math.atan(a.toDouble).toFloat
  def atan2(y: Float, x: Float): Float = Math.atan2(y.toDouble, x.toDouble).toFloat

  def sinh(x: Float): Float = Math.sinh(x.toDouble).toFloat
  def cosh(x: Float): Float = Math.cosh(x.toDouble).toFloat
  def tanh(x: Float): Float = Math.tanh(x.toDouble).toFloat

  def toRadians(a: Float): Float = (a * 2 * pi) / 360
  def toDegrees(a: Float): Float = (a * 360) / (2 * pi)
}

trait FloatIsSigned extends Signed[Float] {
  def signum(a: Float): Int = Math.signum(a).toInt
  def abs(a: Float): Float = if (a < 0.0f) -a else a
}

trait FloatOrder extends Order[Float] {
  override def eqv(x:Float, y:Float) = x == y
  override def neqv(x:Float, y:Float) = x != y
  override def gt(x: Float, y: Float) = x > y
  override def gteqv(x: Float, y: Float) = x >= y
  override def lt(x: Float, y: Float) = x < y
  override def lteqv(x: Float, y: Float) = x <= y
  override def min(x: Float, y: Float) = Math.min(x, y)
  override def max(x: Float, y: Float) = Math.max(x, y)
  def compare(x: Float, y: Float) = java.lang.Float.compare(x, y)
}

trait FloatIsReal extends IsReal[Float] with FloatOrder with FloatIsSigned {
  def toDouble(x: Float): Double = x.toDouble
  def ceil(a:Float): Float = Math.floor(a).toFloat
  def floor(a:Float): Float = Math.floor(a).toFloat
  def round(a:Float): Float = spire.math.round(a)
  def isWhole(a:Float) = a % 1.0 == 0.0
}

@SerialVersionUID(0L)
class FloatAlgebra extends FloatIsField with FloatIsNRoot with FloatIsTrig with FloatIsReal with Serializable

trait FloatInstances {
  implicit final val FloatAlgebra = new FloatAlgebra
}

///////////////////////////////////////////////////////////////////////////////////

// Convertable implementation



trait ConvertableTo[@spec A] {
  def fromByte(n: Byte): A
  def fromShort(n: Short): A
  def fromInt(n: Int): A
  def fromLong(n: Long): A
  def fromFloat(n: Float): A
  def fromDouble(n: Double): A
  def fromBigInt(n: BigInt): A
  def fromBigDecimal(n: BigDecimal): A
  def fromRational(n: Rational): A

  def fromType[B: ConvertableFrom](b: B): A
}

 trait ConvertableToByte extends ConvertableTo[Byte] {
  def fromByte(a: Byte): Byte = a
  def fromShort(a: Short): Byte = a.toByte
  def fromInt(a: Int): Byte = a.toByte
  def fromLong(a: Long): Byte = a.toByte
  def fromFloat(a: Float): Byte = a.toByte
  def fromDouble(a: Double): Byte = a.toByte
  def fromBigInt(a: BigInt): Byte = a.toByte
  def fromBigDecimal(a: BigDecimal): Byte = a.toByte
  def fromRational(a: Rational): Byte = a.toBigInt.toByte

  def fromType[B: ConvertableFrom](b: B): Byte = ConvertableFrom[B].toByte(b)
}

 trait ConvertableToShort extends ConvertableTo[Short] {
  def fromByte(a: Byte): Short = a.toShort
  def fromShort(a: Short): Short = a
  def fromInt(a: Int): Short = a.toShort
  def fromLong(a: Long): Short = a.toShort
  def fromFloat(a: Float): Short = a.toShort
  def fromDouble(a: Double): Short = a.toShort
  def fromBigInt(a: BigInt): Short = a.toShort
  def fromBigDecimal(a: BigDecimal): Short = a.toShort
  def fromRational(a: Rational): Short = a.toBigInt.toShort

  def fromType[B: ConvertableFrom](b: B): Short = ConvertableFrom[B].toShort(b)
}

 trait ConvertableToInt extends ConvertableTo[Int] {
  def fromByte(a: Byte): Int = a.toInt
  def fromShort(a: Short): Int = a.toInt
  def fromInt(a: Int): Int = a
  def fromLong(a: Long): Int = a.toInt
  def fromFloat(a: Float): Int = a.toInt
  def fromDouble(a: Double): Int = a.toInt
  def fromBigInt(a: BigInt): Int = a.toInt
  def fromBigDecimal(a: BigDecimal): Int = a.toInt
  def fromRational(a: Rational): Int = a.toBigInt.toInt

  def fromType[B: ConvertableFrom](b: B): Int = ConvertableFrom[B].toInt(b)
}

 trait ConvertableToLong extends ConvertableTo[Long] {
  def fromByte(a: Byte): Long = a.toLong
  def fromShort(a: Short): Long = a.toLong
  def fromInt(a: Int): Long = a.toLong
  def fromLong(a: Long): Long = a
  def fromFloat(a: Float): Long = a.toLong
  def fromDouble(a: Double): Long = a.toLong
  def fromBigInt(a: BigInt): Long = a.toLong
  def fromBigDecimal(a: BigDecimal): Long = a.toLong
  def fromRational(a: Rational): Long = a.toBigInt.toLong

  def fromType[B: ConvertableFrom](b: B): Long = ConvertableFrom[B].toLong(b)
}

 trait ConvertableToFloat extends ConvertableTo[Float] {
  def fromByte(a: Byte): Float = a.toFloat
  def fromShort(a: Short): Float = a.toFloat
  def fromInt(a: Int): Float = a.toFloat
  def fromLong(a: Long): Float = a.toFloat
  def fromFloat(a: Float): Float = a
  def fromDouble(a: Double): Float = a.toFloat
  def fromBigInt(a: BigInt): Float = a.toFloat
  def fromBigDecimal(a: BigDecimal): Float = a.toFloat
  def fromRational(a: Rational): Float = a.toBigDecimal.toFloat

  def fromType[B: ConvertableFrom](b: B): Float = ConvertableFrom[B].toFloat(b)
}

 trait ConvertableToDouble extends ConvertableTo[Double] {
  def fromByte(a: Byte): Double = a.toDouble
  def fromShort(a: Short): Double = a.toDouble
  def fromInt(a: Int): Double = a.toDouble
  def fromLong(a: Long): Double = a.toDouble
  def fromFloat(a: Float): Double = a.toDouble
  def fromDouble(a: Double): Double = a
  def fromBigInt(a: BigInt): Double = a.toDouble
  def fromBigDecimal(a: BigDecimal): Double = a.toDouble
  def fromRational(a: Rational): Double = a.toBigDecimal.toDouble

  def fromType[B: ConvertableFrom](b: B): Double = ConvertableFrom[B].toDouble(b)
}

 trait ConvertableToBigInt extends ConvertableTo[BigInt] {
  def fromByte(a: Byte): BigInt = BigInt(a)
  def fromShort(a: Short): BigInt = BigInt(a)
  def fromInt(a: Int): BigInt = BigInt(a)
  def fromLong(a: Long): BigInt = BigInt(a)
  def fromFloat(a: Float): BigInt = BigInt(a.toLong)
  def fromDouble(a: Double): BigInt = BigInt(a.toLong)
  def fromBigInt(a: BigInt): BigInt = a
  def fromBigDecimal(a: BigDecimal): BigInt = a.toBigInt
  def fromRational(a: Rational): BigInt = a.toBigInt

  def fromType[B: ConvertableFrom](b: B): BigInt = ConvertableFrom[B].toBigInt(b)
}

 trait ConvertableToBigDecimal extends ConvertableTo[BigDecimal] {
  def fromByte(a: Byte): BigDecimal = BigDecimal(a)
  def fromShort(a: Short): BigDecimal = BigDecimal(a)
  def fromInt(a: Int): BigDecimal = BigDecimal(a)
  def fromLong(a: Long): BigDecimal = BigDecimal(a)
  def fromFloat(a: Float): BigDecimal = BigDecimal(a)
  def fromDouble(a: Double): BigDecimal = BigDecimal(a)
  def fromBigInt(a: BigInt): BigDecimal = BigDecimal(a)
  def fromBigDecimal(a: BigDecimal): BigDecimal = a
  def fromRational(a: Rational): BigDecimal = a.toBigDecimal

  def fromType[B: ConvertableFrom](b: B): BigDecimal = ConvertableFrom[B].toBigDecimal(b)
}

 trait ConvertableToRational extends ConvertableTo[Rational] {
  def fromByte(a: Byte): Rational = Rational(a)
  def fromShort(a: Short): Rational = Rational(a)
  def fromInt(a: Int): Rational = Rational(a)
  def fromLong(a: Long): Rational = Rational(a)
  def fromFloat(a: Float): Rational = Rational(a)
  def fromDouble(a: Double): Rational = Rational(a)
  def fromBigInt(a: BigInt): Rational = Rational(a)
  def fromBigDecimal(a: BigDecimal): Rational = Rational(a)
  def fromRational(a: Rational) = a

  def fromType[B: ConvertableFrom](b: B): Rational = ConvertableFrom[B].toRational(b)
}

 trait ConvertableToAlgebraic extends ConvertableTo[Algebraic] {
  def fromByte(a: Byte): Algebraic = Algebraic(a)
  def fromShort(a: Short): Algebraic = Algebraic(a)
  def fromInt(a: Int): Algebraic = Algebraic(a)
  def fromLong(a: Long): Algebraic = Algebraic(a)
  def fromFloat(a: Float): Algebraic = Algebraic(a)
  def fromDouble(a: Double): Algebraic = Algebraic(a)
  def fromBigInt(a: BigInt): Algebraic = Algebraic(a)
  def fromBigDecimal(a: BigDecimal): Algebraic = Algebraic(a)
  def fromRational(a: Rational) = Algebraic(a)

  def fromType[B: ConvertableFrom](b: B): Algebraic = Algebraic(ConvertableFrom[B].toRational(b))
}


 trait ConvertableToSafeLong extends ConvertableTo[SafeLong] {
  def fromByte(a: Byte): SafeLong = SafeLong(a)
  def fromShort(a: Short): SafeLong = SafeLong(a)
  def fromInt(a: Int): SafeLong = SafeLong(a)
  def fromLong(a: Long): SafeLong = SafeLong(a)
  def fromFloat(a: Float): SafeLong = SafeLong(a.toLong)
  def fromDouble(a: Double): SafeLong = SafeLong(a.toLong)
  def fromBigInt(a: BigInt): SafeLong = SafeLong(a)
  def fromBigDecimal(a: BigDecimal): SafeLong = SafeLong(a.toBigInt)
  def fromRational(a: Rational): SafeLong = SafeLong(a.toBigInt)

  def fromType[B: ConvertableFrom](b: B): SafeLong = SafeLong(ConvertableFrom[B].toBigInt(b))
}

trait ConvertableToNumber extends ConvertableTo[Number] {
  def fromByte(a: Byte): Number = Number(a)
  def fromShort(a: Short): Number = Number(a)
  def fromInt(a: Int): Number = Number(a)
  def fromLong(a: Long): Number = Number(a)
  def fromFloat(a: Float): Number = Number(a)
  def fromDouble(a: Double): Number = Number(a)
  def fromBigInt(a: BigInt): Number = Number(a)
  def fromBigDecimal(a: BigDecimal): Number = Number(a)
  def fromRational(a: Rational): Number = Number(a)

  def fromType[B: ConvertableFrom](b: B): Number = Number(ConvertableFrom[B].toDouble(b))
}

 trait ConvertableToNatural extends ConvertableTo[Natural] {
  def fromByte(a: Byte): Natural = Natural(a)
  def fromShort(a: Short): Natural = Natural(a)
  def fromInt(a: Int): Natural = Natural(a)
  def fromLong(a: Long): Natural = Natural(a)
  def fromFloat(a: Float): Natural = Natural(BigDecimal(a).toBigInt)
  def fromDouble(a: Double): Natural = Natural(BigDecimal(a).toBigInt)
  def fromBigInt(a: BigInt): Natural = Natural(a)
  def fromBigDecimal(a: BigDecimal): Natural = Natural(a.toBigInt)
  def fromRational(a: Rational): Natural = Natural(a.toBigInt)

  def fromType[B: ConvertableFrom](b: B): Natural = Natural(ConvertableFrom[B].toBigInt(b))
}

object ConvertableTo {
  @inline final def apply[A](implicit ev: ConvertableTo[A]) = ev

  implicit final val ConvertableToByte = new ConvertableToByte {}
  implicit final val ConvertableToShort = new ConvertableToShort {}
  implicit final val ConvertableToInt = new ConvertableToInt {}
  implicit final val ConvertableToLong = new ConvertableToLong {}
  implicit final val ConvertableToBigInt = new ConvertableToBigInt {}
  implicit final val ConvertableToFloat = new ConvertableToFloat {}
  implicit final val ConvertableToDouble = new ConvertableToDouble {}
  implicit final val ConvertableToBigDecimal = new ConvertableToBigDecimal {}
  implicit final val ConvertableToRational = new ConvertableToRational {}
  implicit final val ConvertableToAlgebraic = new ConvertableToAlgebraic {}
  implicit final val ConvertableToSafeLong = new ConvertableToSafeLong {}
  implicit final val ConvertableToNumber = new ConvertableToNumber {}
  implicit final val ConvertableToNatural = new ConvertableToNatural {}

 
}

trait ConvertableFrom[@spec A] {
  def toByte(a: A): Byte
  def toShort(a: A): Short
  def toInt(a: A): Int
  def toLong(a: A): Long
  def toFloat(a: A): Float
  def toDouble(a: A): Double
  def toBigInt(a: A): BigInt
  def toBigDecimal(a: A): BigDecimal
  def toRational(a: A): Rational
  def toNumber(a: A): Number

  def toType[B: ConvertableTo](a: A): B
  def toString(a: A): String
}

 trait ConvertableFromByte extends ConvertableFrom[Byte] {
  def toByte(a: Byte): Byte = a
  def toShort(a: Byte): Short = a.toShort
  def toInt(a: Byte): Int = a.toInt
  def toLong(a: Byte): Long = a.toLong
  def toFloat(a: Byte): Float = a.toFloat
  def toDouble(a: Byte): Double = a.toDouble
  def toBigInt(a: Byte): BigInt = BigInt(a)
  def toBigDecimal(a: Byte): BigDecimal = BigDecimal(a)
  def toRational(a: Byte): Rational = Rational(a)
  def toNumber(a: Byte): Number = Number(a)

  def toType[B: ConvertableTo](a: Byte): B = ConvertableTo[B].fromByte(a)
  def toString(a: Byte): String = a.toString
}

 trait ConvertableFromShort extends ConvertableFrom[Short] {
  def toByte(a: Short): Byte = a.toByte
  def toShort(a: Short): Short = a
  def toInt(a: Short): Int = a.toInt
  def toLong(a: Short): Long = a.toLong
  def toFloat(a: Short): Float = a.toFloat
  def toDouble(a: Short): Double = a.toDouble
  def toBigInt(a: Short): BigInt = BigInt(a)
  def toBigDecimal(a: Short): BigDecimal = BigDecimal(a)
  def toRational(a: Short): Rational = Rational(a)
  def toNumber(a: Short): Number = Number(a)

  def toType[B: ConvertableTo](a: Short): B = ConvertableTo[B].fromShort(a)
  def toString(a: Short): String = a.toString
}

 trait ConvertableFromInt extends ConvertableFrom[Int] {
  def toByte(a: Int): Byte = a.toByte
  def toShort(a: Int): Short = a.toShort
  def toInt(a: Int): Int = a
  def toLong(a: Int): Long = a.toLong
  def toFloat(a: Int): Float = a.toFloat
  def toDouble(a: Int): Double = a.toDouble
  def toBigInt(a: Int): BigInt = BigInt(a)
  def toBigDecimal(a: Int): BigDecimal = BigDecimal(a)
  def toRational(a: Int): Rational = Rational(a)
  def toNumber(a: Int): Number = Number(a)

  def toType[B: ConvertableTo](a: Int): B = ConvertableTo[B].fromInt(a)
  def toString(a: Int): String = a.toString
}

trait ConvertableFromLong extends ConvertableFrom[Long] {
  def toByte(a: Long): Byte = a.toByte
  def toShort(a: Long): Short = a.toShort
  def toInt(a: Long): Int = a.toInt
  def toLong(a: Long): Long = a
  def toFloat(a: Long): Float = a.toFloat
  def toDouble(a: Long): Double = a.toDouble
  def toBigInt(a: Long): BigInt = BigInt(a)
  def toBigDecimal(a: Long): BigDecimal = BigDecimal(a)
  def toRational(a: Long): Rational = Rational(a)
  def toNumber(a: Long): Number = Number(a)

  def toType[B: ConvertableTo](a: Long): B = ConvertableTo[B].fromLong(a)
  def toString(a: Long): String = a.toString
}

trait ConvertableFromFloat extends ConvertableFrom[Float] {
  def toByte(a: Float): Byte = a.toByte
  def toShort(a: Float): Short = a.toShort
  def toInt(a: Float): Int = a.toInt
  def toLong(a: Float): Long = a.toLong
  def toFloat(a: Float): Float = a
  def toDouble(a: Float): Double = a.toDouble
  def toBigInt(a: Float): BigInt = BigInt(a.toLong)
  def toBigDecimal(a: Float): BigDecimal = BigDecimal(a)
  def toRational(a: Float): Rational = Rational(a)
  def toNumber(a: Float): Number = Number(a)

  def toType[B: ConvertableTo](a: Float): B = ConvertableTo[B].fromFloat(a)
  def toString(a: Float): String = a.toString
}

trait ConvertableFromDouble extends ConvertableFrom[Double] {
  def toByte(a: Double): Byte = a.toByte
  def toShort(a: Double): Short = a.toShort
  def toInt(a: Double): Int = a.toInt
  def toLong(a: Double): Long = a.toLong
  def toFloat(a: Double): Float = a.toFloat
  def toDouble(a: Double): Double = a
  def toBigInt(a: Double): BigInt = BigInt(a.toLong)
  def toBigDecimal(a: Double): BigDecimal = BigDecimal(a)
  def toRational(a: Double): Rational = Rational(a)
  def toNumber(a: Double): Number = Number(a)

  def toType[B: ConvertableTo](a: Double): B = ConvertableTo[B].fromDouble(a)
  def toString(a: Double): String = a.toString
}

trait ConvertableFromBigInt extends ConvertableFrom[BigInt] {
  def toByte(a: BigInt): Byte = a.toByte
  def toShort(a: BigInt): Short = a.toShort
  def toInt(a: BigInt): Int = a.toInt
  def toLong(a: BigInt): Long = a.toLong
  def toFloat(a: BigInt): Float = a.toFloat
  def toDouble(a: BigInt): Double = a.toDouble
  def toBigInt(a: BigInt): BigInt = a
  def toBigDecimal(a: BigInt): BigDecimal = BigDecimal(a)
  def toRational(a: BigInt): Rational = Rational(a)
  def toNumber(a: BigInt): Number = Number(a)

  def toType[B: ConvertableTo](a: BigInt): B = ConvertableTo[B].fromBigInt(a)
  def toString(a: BigInt): String = a.toString
}

trait ConvertableFromBigDecimal extends ConvertableFrom[BigDecimal] {
  def toByte(a: BigDecimal): Byte = a.toByte
  def toShort(a: BigDecimal): Short = a.toShort
  def toInt(a: BigDecimal): Int = a.toInt
  def toLong(a: BigDecimal): Long = a.toLong
  def toFloat(a: BigDecimal): Float = a.toFloat
  def toDouble(a: BigDecimal): Double = a.toDouble
  def toBigInt(a: BigDecimal): BigInt = a.toBigInt
  def toBigDecimal(a: BigDecimal): BigDecimal = a
  def toRational(a: BigDecimal): Rational = Rational(a)
  def toNumber(a: BigDecimal): Number = Number(a)

  def toType[B: ConvertableTo](a: BigDecimal): B = ConvertableTo[B].fromBigDecimal(a)
  def toString(a: BigDecimal): String = a.toString
}

trait ConvertableFromRational extends ConvertableFrom[Rational] {
  def toByte(a: Rational): Byte = a.toBigInt.toByte
  def toShort(a: Rational): Short = a.toBigInt.toShort
  def toInt(a: Rational): Int = a.toBigInt.toInt
  def toLong(a: Rational): Long = a.toBigInt.toLong
  def toFloat(a: Rational): Float = a.toBigDecimal.toFloat
  def toDouble(a: Rational): Double = a.toBigDecimal.toDouble
  def toBigInt(a: Rational): BigInt = a.toBigInt
  def toBigDecimal(a: Rational): BigDecimal = a.toBigDecimal
  def toRational(a: Rational): Rational = a
  def toNumber(a: Rational): Number = Number(a.toBigDecimal)

  def toType[B: ConvertableTo](a: Rational): B = ConvertableTo[B].fromRational(a)
  def toString(a: Rational): String = a.toString
}

trait ConvertableFromAlgebraic extends ConvertableFrom[Algebraic] {
  def toByte(a: Algebraic): Byte = a.toInt.toByte
  def toShort(a: Algebraic): Short = a.toInt.toShort
  def toInt(a: Algebraic): Int = a.toInt
  def toLong(a: Algebraic): Long = a.toLong
  def toFloat(a: Algebraic): Float = a.toDouble.toFloat
  def toDouble(a: Algebraic): Double = a.toDouble
  def toBigInt(a: Algebraic): BigInt = a.toBigInt
  // TODO: Figure out how to deal with variable approximability.
  def toBigDecimal(a: Algebraic): BigDecimal = a.toBigDecimal(java.math.MathContext.DECIMAL128)
  def toRational(a: Algebraic): Rational = a.toRational(ApproximationContext(Rational(1L, 100000000000000000L)))
  def toNumber(a: Algebraic): Number = Number(toBigDecimal(a))

  def toType[B: ConvertableTo](a: Algebraic): B = ConvertableTo[B].fromRational(a.toRational)
  def toString(a: Algebraic): String = a.toString
}

trait ConvertableFromComplex[A] extends ConvertableFrom[Complex[A]] {
  def algebra: Integral[A]

  def toByte(a: Complex[A]): Byte = algebra.toByte(a.real)
  def toShort(a: Complex[A]): Short = algebra.toShort(a.real)
  def toInt(a: Complex[A]): Int = algebra.toInt(a.real)
  def toLong(a: Complex[A]): Long = algebra.toLong(a.real)
  def toFloat(a: Complex[A]): Float = algebra.toFloat(a.real)
  def toDouble(a: Complex[A]): Double = algebra.toDouble(a.real)
  def toBigInt(a: Complex[A]): BigInt = algebra.toBigInt(a.real)
  def toBigDecimal(a: Complex[A]): BigDecimal = algebra.toBigDecimal(a.real)
  def toRational(a: Complex[A]): Rational = algebra.toRational(a.real)
  def toNumber(a: Complex[A]): Number = algebra.toNumber(a.real)

  def toType[B: ConvertableTo](a: Complex[A]): B = sys.error("fixme")
  def toString(a: Complex[A]): String = a.toString
}

trait ConvertableFromSafeLong extends ConvertableFrom[SafeLong] {
  def toByte(a: SafeLong): Byte = a.toBigInt.toByte
  def toShort(a: SafeLong): Short = a.toBigInt.toShort
  def toInt(a: SafeLong): Int = a.toBigInt.toInt
  def toLong(a: SafeLong): Long = a.toBigInt.toLong
  def toFloat(a: SafeLong): Float = a.toBigInt.toFloat
  def toDouble(a: SafeLong): Double = a.toBigInt.toDouble
  def toBigInt(a: SafeLong): BigInt = a.toBigInt
  def toBigDecimal(a: SafeLong): BigDecimal = BigDecimal(a.toBigInt)
  def toRational(a: SafeLong): Rational = Rational(a.toBigInt)
  def toNumber(a: SafeLong): Number = Number(a)

  def toType[B: ConvertableTo](a: SafeLong): B = ConvertableTo[B].fromBigInt(a.toBigInt)
  def toString(a: SafeLong): String = a.toString
}

trait ConvertableFromNumber extends ConvertableFrom[Number] {
  def toByte(a: Number): Byte = a.toBigInt.toByte
  def toShort(a: Number): Short = a.toBigInt.toShort
  def toInt(a: Number): Int = a.toBigInt.toInt
  def toLong(a: Number): Long = a.toBigInt.toLong
  def toFloat(a: Number): Float = a.toBigInt.toFloat
  def toDouble(a: Number): Double = a.toBigInt.toDouble
  def toBigInt(a: Number): BigInt = a.toBigInt
  def toBigDecimal(a: Number): BigDecimal = BigDecimal(a.toBigInt)
  def toRational(a: Number): Rational = Rational(a.toBigInt)
  def toNumber(a: Number): Number = a

  def toType[B: ConvertableTo](a: Number): B = ConvertableTo[B].fromBigInt(a.toBigInt)
  def toString(a: Number): String = a.toString
}

trait ConvertableFromNatural extends ConvertableFrom[Natural] {
  def toByte(a: Natural): Byte = a.toBigInt.toByte
  def toShort(a: Natural): Short = a.toBigInt.toShort
  def toInt(a: Natural): Int = a.toBigInt.toInt
  def toLong(a: Natural): Long = a.toBigInt.toLong
  def toFloat(a: Natural): Float = a.toBigInt.toFloat
  def toDouble(a: Natural): Double = a.toBigInt.toDouble
  def toBigInt(a: Natural): BigInt = a.toBigInt
  def toBigDecimal(a: Natural): BigDecimal = BigDecimal(a.toBigInt)
  def toRational(a: Natural): Rational = Rational(a.toBigInt)
  def toNumber(a: Natural): Number = Number(a.toBigInt)

  def toType[B: ConvertableTo](a: Natural): B = ConvertableTo[B].fromBigInt(a.toBigInt)
  def toString(a: Natural): String = a.toString
}


object ConvertableFrom {
  @inline final def apply[A](implicit ev: ConvertableFrom[A]) = ev

  implicit final val ConvertableFromByte = new ConvertableFromByte {}
  implicit final val ConvertableFromShort = new ConvertableFromShort {}
  implicit final val ConvertableFromInt = new ConvertableFromInt {}
  implicit final val ConvertableFromLong = new ConvertableFromLong {}
  implicit final val ConvertableFromFloat = new ConvertableFromFloat {}
  implicit final val ConvertableFromDouble = new ConvertableFromDouble {}
  implicit final val ConvertableFromBigInt = new ConvertableFromBigInt {}
  implicit final val ConvertableFromBigDecimal = new ConvertableFromBigDecimal {}
  implicit final val ConvertableFromRational = new ConvertableFromRational {}
  implicit final val ConvertableFromAlgebraic = new ConvertableFromAlgebraic {}
  implicit final val ConvertableFromSafeLong = new ConvertableFromSafeLong {}
  implicit final val ConvertableFromNumber = new ConvertableFromNumber {}
  implicit final val ConvertableFromNatural = new ConvertableFromNatural {}

}

//////////////////////////////////////////////////////////////////////////////////////

// Ring, Rig, Rng, and Semiring


//Ring
trait Ring[@spec(Byte, Short, Int, Long, Float, Double) A] extends Rig[A] with Rng[A] {
  def fromInt(n: Int): A = additive.sumn(one, n)
}

object Ring {
  @inline final def apply[A](implicit r: Ring[A]): Ring[A] = r
}

trait CRing[@spec(Byte, Short, Int, Long, Float, Double) A] extends Ring[A] with MultiplicativeCMonoid[A]

object CRing {
  @inline final def apply[A](implicit r: CRing[A]): CRing[A] = r
}


//Rig
trait Rig[@spec(Byte, Short, Int, Long, Float, Double) A] extends Semiring[A] with AdditiveMonoid[A] with MultiplicativeMonoid[A] {
  override def pow(a:A, n:Int):A =
    if (n >= 0) multiplicative.sumn(a, n)
    else throw new IllegalArgumentException(s"Illegal negative exponent $n to Monoid#pow")
}

object Rig {
  @inline final def apply[A](implicit r:Rig[A]): Rig[A] = r
}

//Rng

trait Rng[@spec(Byte, Short, Int, Long, Float, Double) A] extends Semiring[A] with AdditiveAbGroup[A]

object Rng {
  @inline final def apply[A](implicit r:Rng[A]):Rng[A] = r
}

//Semiring

trait Semiring[@spec(Byte, Short, Int, Long, Float, Double) A] extends AdditiveMonoid[A] with MultiplicativeSemigroup[A] {
  def pow(a:A, n:Int):A =
    if (n > 0) multiplicative.sumn(a, n)
    else throw new IllegalArgumentException(s"Illegal non-positive exponent $n to Semiring#pow")
}

object Semiring {
  @inline final def apply[A](implicit r:Semiring[A]):Semiring[A] = r
}

//////////////////////////////////////////////////////////////////////////////////////////

// Aditive things, We need most of them


object Additive {
  def apply[A](s: Semigroup[A]): AdditiveSemigroup[A] = new AdditiveSemigroup[A] {
    def plus(x: A, y: A): A = s.op(x, y)
  }

  def apply[A](s: CSemigroup[A]): AdditiveCSemigroup[A] = new AdditiveCSemigroup[A] {
    def plus(x: A, y: A): A = s.op(x, y)
  }

  def apply[A](m: Monoid[A]): AdditiveMonoid[A] = new AdditiveMonoid[A] {
    def plus(x: A, y: A): A = m.op(x, y)
    def zero = m.id
  }

  def apply[A](m: CMonoid[A]): AdditiveCMonoid[A] = new AdditiveCMonoid[A] {
    def plus(x: A, y: A): A = m.op(x, y)
    def zero = m.id
  }

  def apply[A](g: Group[A]): AdditiveGroup[A] = new AdditiveGroup[A] {
    def plus(x: A, y: A): A = g.op(x, y)
    override def minus(x: A, y: A): A = g.op(x, g.inverse(y))
    def zero: A = g.id
    def negate(x: A): A = g.inverse(x)
  }

  def apply[A](g: AbGroup[A]): AdditiveAbGroup[A] = new AdditiveAbGroup[A] {
    def plus(x: A, y: A): A = g.op(x, y)
    override def minus(x: A, y: A): A = g.op(x, g.inverse(y))
    def zero: A = g.id
    def negate(x: A): A = g.inverse(x)
  }
}

trait AdditiveSemigroup[@spec(Byte, Short, Int, Long, Float, Double) A] {
  def additive: Semigroup[A] = new Semigroup[A] {
    def op(x: A, y: A): A = plus(x, y)
  }

  def plus(x: A, y: A): A
}

trait AdditiveCSemigroup[@spec(Byte, Short, Int, Long, Float, Double) A] extends AdditiveSemigroup[A] {
  override def additive: CSemigroup[A] = new CSemigroup[A] {
    def op(x: A, y: A): A = plus(x, y)
  }
}

trait AdditiveMonoid[@spec(Byte, Short, Int, Long, Float, Double) A] extends AdditiveSemigroup[A] {
  override def additive: Monoid[A] = new Monoid[A] {
    def id = zero
    def op(x: A, y: A): A = plus(x, y)
  }

  def zero: A
}

trait AdditiveCMonoid[@spec(Byte, Short, Int, Long, Float, Double) A] extends AdditiveMonoid[A] with AdditiveCSemigroup[A] {
  override def additive: CMonoid[A] = new CMonoid[A] {
    def id = zero
    def op(x: A, y: A): A = plus(x, y)
  }
}

trait AdditiveGroup[@spec(Byte, Short, Int, Long, Float, Double) A] extends AdditiveMonoid[A] {
  override def additive: Group[A] = new Group[A] {
    def id = zero
    def op(x: A, y: A): A = plus(x, y)
    def inverse(x: A): A = negate(x)
  }

  def negate(x: A): A
  def minus(x: A, y: A): A = plus(x, negate(y))
}

trait AdditiveAbGroup[@spec(Byte, Short, Int, Long, Float, Double) A] extends AdditiveGroup[A] with AdditiveCMonoid[A] {
  override def additive: AbGroup[A] = new AbGroup[A] {
    def id = zero
    def op(x: A, y: A): A = plus(x, y)
    def inverse(x: A): A = negate(x)
  }
}
//////////////////////////////////////////////////////////////////////////////////////////

//Semigroup


trait Semigroup[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A] {
  def op(x: A, y: A): A

 
  def sumn(a: A, n: Int): A =
    if (n <= 0) throw new IllegalArgumentException("Repeated summation for semigroups must have reptitions > 0")
    else if (n == 1) a
    else sumnAboveOne(a, n)

  protected def sumnAboveOne(a: A, n: Int): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A =
      if (k == 1) {
        op(b, extra)
      } else {
        val x = if ((k & 1) == 1) op(b, extra) else extra
        loop(op(b, b), k >>> 1, x)
      }
    loop(a, n - 1, a)
  }

  
  def sumOption(as: TraversableOnce[A]): Option[A] = as.reduceOption(op)
}

object Semigroup {
  @inline final def apply[A](implicit s: Semigroup[A]) = s

  
  @inline final def additive[A](implicit A: AdditiveSemigroup[A]) =  A.additive

 
  @inline final def multiplicative[A](implicit A: MultiplicativeSemigroup[A]) = A.multiplicative
}

trait CSemigroup[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A]
    extends Semigroup[A]

object CSemigroup {
  @inline final def apply[A](implicit ev: CSemigroup[A]): CSemigroup[A] = ev
  @inline final def additive[A](implicit A: AdditiveCSemigroup[A]): CSemigroup[A] =  A.additive
  @inline final def multiplicative[A](implicit A: MultiplicativeCSemigroup[A]): CSemigroup[A] = A.multiplicative
}


//////////////////////////////////////////////////////////////////////////////////////////

//Monoid

trait Monoid[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A]
    extends Semigroup[A] {
  def id: A
  override def sumn(a: A, n: Int): A =
    if (n < 0) throw new IllegalArgumentException("Repeated summation for monoids must have reptitions >= 0")
    else if (n == 0) id
    else if (n == 1) a
    else sumnAboveOne(a, n)
  def sum(as: TraversableOnce[A]): A = as.reduce(op)
}

object Monoid {
  @inline final def apply[A](implicit m: Monoid[A]): Monoid[A] = m
  @inline final def additive[A](implicit A: AdditiveMonoid[A]) = A.additive
  @inline final def multiplicative[A](implicit A: MultiplicativeMonoid[A]) = A.multiplicative

}

trait CMonoid[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A]
    extends Monoid[A] with CSemigroup[A]

object CMonoid {
  @inline final def apply[A](implicit ev: CMonoid[A]): CMonoid[A] = ev
  @inline final def additive[A](implicit A: AdditiveCMonoid[A]): CMonoid[A] =  A.additive
  @inline final def multiplicative[A](implicit A: MultiplicativeCMonoid[A]): CMonoid[A] = A.multiplicative
}

//////////////////////////////////////////////////////////////////////////////////////////

//Group

trait Group[@spec(Byte, Short, Int, Long, Float, Double) A]
    extends Monoid[A] {

  def inverse(a: A): A

  def opInverse(a: A, b: A): A = op(a, inverse(b))
  
  override def sumn(a: A, n: Int): A =
    if (n == Int.MinValue) op(sumn(inverse(a), Int.MaxValue), inverse(a))
    else if (n < 0) sumn(inverse(a), -n)
    else if (n == 0) id
    else if (n == 1) a
    else sumnAboveOne(a, n)
}

object Group {
  @inline final def apply[A](implicit ev: Group[A]): Group[A] = ev
  @inline final def additive[A](implicit A: AdditiveGroup[A]): Group[A] =  A.additive
  @inline final def multiplicative[A](implicit A: MultiplicativeGroup[A]): Group[A] = A.multiplicative
}


trait AbGroup[@spec(Byte, Short, Int, Long, Float, Double) A]
    extends Group[A] with CMonoid[A]

object AbGroup {
  @inline final def apply[A](implicit ev: AbGroup[A]): AbGroup[A] = ev
  @inline final def additive[A](implicit A: AdditiveAbGroup[A]): AbGroup[A] =  A.additive
  @inline final def multiplicative[A](implicit A: MultiplicativeAbGroup[A]): AbGroup[A] = A.multiplicative
}



/////////////////////////////////////////////////////////////////////////////////////////

// Multiplicative

object Multiplicative {
  def apply[A](s: Semigroup[A]): MultiplicativeSemigroup[A] = new MultiplicativeSemigroup[A] {
    def times(x: A, y: A): A = s.op(x, y)
  }

  def apply[A](s: CSemigroup[A]): MultiplicativeCSemigroup[A] = new MultiplicativeCSemigroup[A] {
    def times(x: A, y: A): A = s.op(x, y)
  }

  def apply[A](m: Monoid[A]): MultiplicativeMonoid[A] = new MultiplicativeMonoid[A] {
    def times(x: A, y: A): A = m.op(x, y)
    def one = m.id
  }

  def apply[A](m: CMonoid[A]): MultiplicativeCMonoid[A] = new MultiplicativeCMonoid[A] {
    def times(x: A, y: A): A = m.op(x, y)
    def one = m.id
  }

  def apply[A](g: Group[A]): MultiplicativeGroup[A] = new MultiplicativeGroup[A] {
    def times(x: A, y: A): A = g.op(x, y)
    def div(x: A, y: A): A = g.op(x, g.inverse(y))
    def one: A = g.id
    override def reciprocal(x: A): A = g.inverse(x)
  }

  def apply[A](g: AbGroup[A]): MultiplicativeAbGroup[A] = new MultiplicativeAbGroup[A] {
    def times(x: A, y: A): A = g.op(x, y)
    def div(x: A, y: A): A = g.op(x, g.inverse(y))
    def one: A = g.id
    override def reciprocal(x: A): A = g.inverse(x)
  }
}

trait MultiplicativeSemigroup[@spec(Byte, Short, Int, Long, Float, Double) A] {
  def multiplicative: Semigroup[A] = new Semigroup[A] {
    def op(x: A, y: A): A = times(x, y)
  }

  def times(x: A, y: A): A
}

trait MultiplicativeCSemigroup[@spec(Byte, Short, Int, Long, Float, Double) A] extends MultiplicativeSemigroup[A] {
  override def multiplicative: CSemigroup[A] = new CSemigroup[A] {
    def op(x: A, y: A): A = times(x, y)
  }
}

trait MultiplicativeMonoid[@spec(Byte, Short, Int, Long, Float, Double) A] extends MultiplicativeSemigroup[A] {
  override def multiplicative: Monoid[A] = new Monoid[A] {
    def id = one
    def op(x: A, y: A): A = times(x, y)
  }

  def one: A
}

trait MultiplicativeCMonoid[@spec(Byte, Short, Int, Long, Float, Double) A] extends MultiplicativeMonoid[A] with MultiplicativeCSemigroup[A] {
  override def multiplicative: CMonoid[A] = new CMonoid[A] {
    def id = one
    def op(x: A, y: A): A = times(x, y)
  }
}

trait MultiplicativeGroup[@spec(Byte, Short, Int, Long, Float, Double) A] extends MultiplicativeMonoid[A] {
  override def multiplicative: Group[A] = new Group[A] {
    def id = one
    def op(x: A, y: A): A = times(x, y)
    def inverse(x: A): A = reciprocal(x)
  }

  def reciprocal(x: A): A = div(one, x)
  def div(x: A, y: A): A
}

trait MultiplicativeAbGroup[@spec(Byte, Short, Int, Long, Float, Double) A] extends MultiplicativeGroup[A] with MultiplicativeCMonoid[A] {
  override def multiplicative: AbGroup[A] = new AbGroup[A] {
    def id = one
    def op(x: A, y: A): A = times(x, y)
    def inverse(x: A): A = reciprocal(x)
  }
}

