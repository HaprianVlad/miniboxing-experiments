package project_macros_miniboxed


import scala.{specialized => spec}
import scala.reflect.ClassTag
import scala.language.implicitConversions

// Imports used for Private Spire Implementation
import java.lang.Math
import java.lang.Long.{ numberOfTrailingZeros, numberOfLeadingZeros }
import java.lang.Float.{ intBitsToFloat, floatToIntBits }
import java.lang.Double.{ isInfinite, isNaN, doubleToLongBits,longBitsToDouble }
import scala.annotation.{ switch, tailrec }
import java.math.MathContext
import scala.math.{ScalaNumber, ScalaNumericConversions, ScalaNumericAnyConversions}




//******************************************************************//

// This part consists on isolating the needed spire implementation for this benchmark

//*****************************************************************//

// 1. Numeric

trait Numeric[@miniboxed A] extends Ring[A]
with AdditiveAbGroup[A] with MultiplicativeAbGroup[A] with NRoot[A]
with ConvertableFrom[A] with ConvertableTo[A] with IsReal[A] 

object Numeric {
  
  implicit final val IntIsNumeric: Numeric[Int] = new IntIsNumeric
  implicit final val LongIsNumeric: Numeric[Long] = new LongIsNumeric
  implicit final val FloatIsNumeric: Numeric[Float] = new FloatIsNumeric
  implicit final val DoubleIsNumeric: Numeric[Double] = new DoubleIsNumeric
  /*implicit final val BigIntIsNumeric: Numeric[BigInt] = new BigIntIsNumeric
  implicit final val BigDecimalIsNumeric: Numeric[BigDecimal] = new BigDecimalIsNumeric
  implicit final val AlgebraicIsNumeric: Numeric[Algebraic] = new AlgebraicIsNumeric
  implicit final val RealIsNumeric: Numeric[Real] = new RealIsNumeric

  private val defaultApprox = ApproximationContext(Rational(1, 1000000000))

  implicit def RationalIsNumeric(implicit ctx: ApproximationContext[Rational] = defaultApprox): Numeric[Rational] =
    new RationalIsNumeric

  implicit def complexIsNumeric[A: Fractional: Trig: IsReal] = new ComplexIsNumeric
*/
  @inline final def apply[A](implicit ev: Numeric[A]):Numeric[A] = ev
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

///////////////////////////////////////////////////////////////////////////
//BitString and BooleanAlgebra implementation

trait BooleanAlgebra[@miniboxed A] { self =>
  def one: A
  def zero: A
  def complement(a: A): A
  def and(a: A, b: A): A
  def or(a: A, b: A): A
  def xor(a: A, b: A): A = or(and(a, complement(b)), and(complement(a), b))

  def imp(a: A, b: A): A = or(complement(a), b)
  def nand(a: A, b: A): A = complement(and(a, b))
  def nor(a: A, b: A): A = complement(or(a, b))
  def nxor(a: A, b: A): A = and(or(a, complement(b)), or(complement(a), b))

  def dual: BooleanAlgebra[A] = new BooleanAlgebra[A] {
    def one: A = self.zero
    def zero: A = self.one
    def and(a: A, b: A): A = self.or(a, b)
    def or(a: A, b: A): A = self.and(a, b)
    def complement(a: A): A = self.complement(a)
    override def xor(a: A, b: A): A = self.complement(self.xor(a, b))

    override def dual: BooleanAlgebra[A] = self
  }
}

object BooleanAlgebra {
  @inline final def apply[@miniboxed A](
    implicit ev: BooleanAlgebra[A]): BooleanAlgebra[A] = ev
}


trait BitString[@miniboxed A] extends BooleanAlgebra[A] {
  def signed: Boolean
  def width: Int
  def toHexString(n: A): String

  def bitCount(n: A): Int
  def highestOneBit(n: A): A
  def lowestOneBit(n: A): A
  def numberOfLeadingZeros(n: A): Int
  def numberOfTrailingZeros(n: A): Int

  def leftShift(n: A, i: Int): A
  def rightShift(n: A, i: Int): A
  def signedRightShift(n: A, i: Int): A
  def rotateLeft(n: A, i: Int): A
  def rotateRight(n: A, i: Int): A
}

object BitString {
  def apply[A](implicit ev: BitString[A]): BitString[A] = ev
}
///////////////////////////////////////////////////////////////////////////
//Long implementation

trait LongIsEuclideanRing extends EuclideanRing[Long] {
  override def minus(a:Long, b:Long): Long = a - b
  def negate(a:Long): Long = -a
  def one: Long = 1L
  def plus(a:Long, b:Long): Long = a + b
  override def pow(a: Long, b:Int): Long = b match {
    case 0 => 1
    case 1 => a
    case 2 => a * a
    case 3 => a * a * a
    case _ =>
      if (b > 0) {
        val e = b >> 1
        val c = if ((b & 1) == 1) a else 1
        c * pow(a, e) * pow(a, e)
      } else {
        0
      }
  }
  override def times(a:Long, b:Long): Long = a * b
  def zero: Long = 0L
  
  override def fromInt(n: Int): Long = n

  def quot(a:Long, b:Long) = a / b
  def mod(a:Long, b:Long) = a % b
  def gcd(a:Long, b:Long) = math.gcd(a, b)
}

// Not included in Instances trait!
trait LongIsNRoot extends NRoot[Long] {
  def nroot(x: Long, n: Int): Long = {
    def findnroot(prev: Long, add: Long): Long = {
      val next = prev | add
      val e = Math.pow(next, n)

      if (e == x || add == 0) {
        next
      } else if (e <= 0 || e > x) {
        findnroot(prev, add >> 1)
      } else {
        findnroot(next, add >> 1)
      }
    }

    if (n < 1) throw new IllegalArgumentException(s"nroot($n)")
    else if (n == 1) x
    else findnroot(0, 1L << ((65 - n) / n))
  }
  def log(a:Long) = Math.log(a.toDouble).toLong
  def fpow(a:Long, b:Long) = math.pow(a, b) // xyz
}

trait LongOrder extends Order[Long] {
  override def eqv(x:Long, y:Long) = x == y
  override def neqv(x:Long, y:Long) = x != y
  override def gt(x: Long, y: Long) = x > y
  override def gteqv(x: Long, y: Long) = x >= y
  override def lt(x: Long, y: Long) = x < y
  override def lteqv(x: Long, y: Long) = x <= y
  def compare(x: Long, y: Long) = if (x < y) -1 else if (x == y) 0 else 1
}

trait LongIsSigned extends Signed[Long] {
  def signum(a: Long): Int = java.lang.Long.signum(a)
  def abs(a: Long): Long = if (a < 0L) -a else a
}

trait LongIsReal extends IsIntegral[Long] with LongOrder with LongIsSigned {
  def toDouble(n: Long): Double = n.toDouble
}

@SerialVersionUID(0L)
class LongIsBitString extends BitString[Long] with Serializable {
  def one: Long = -1L
  def zero: Long = 0L
  def and(a: Long, b: Long): Long = a & b
  def or(a: Long, b: Long): Long = a | b
  def complement(a: Long): Long = ~a
  override def xor(a: Long, b: Long): Long = a ^ b

  def signed: Boolean = true
  def width: Int = 64
  def toHexString(n: Long): String = java.lang.Long.toHexString(n)

  def bitCount(n: Long): Int = java.lang.Long.bitCount(n)
  def highestOneBit(n: Long): Long = java.lang.Long.highestOneBit(n)
  def lowestOneBit(n: Long): Long = java.lang.Long.lowestOneBit(n)
  def numberOfLeadingZeros(n: Long): Int = java.lang.Long.numberOfLeadingZeros(n)
  def numberOfTrailingZeros(n: Long): Int = java.lang.Long.numberOfTrailingZeros(n)

  def leftShift(n: Long, i: Int): Long = n << i
  def rightShift(n: Long, i: Int): Long = n >> i
  def signedRightShift(n: Long, i: Int): Long = n >>> i
  def rotateLeft(n: Long, i: Int): Long = java.lang.Long.rotateLeft(n, i)
  def rotateRight(n: Long, i: Int): Long = java.lang.Long.rotateRight(n, i)
}

@SerialVersionUID(0L)
class LongAlgebra extends LongIsEuclideanRing with LongIsNRoot with LongIsReal with Serializable

trait LongInstances {
  implicit final val LongBitString = new LongIsBitString
  implicit final val LongAlgebra = new LongAlgebra
}

///////////////////////////////////////////////////////////////////////////
//Int implementation

trait IntIsEuclideanRing extends EuclideanRing[Int] {
  override def minus(a:Int, b:Int): Int = a - b
  def negate(a:Int): Int = -a
  def one: Int = 1
  def plus(a:Int, b:Int): Int = a + b
  override def pow(a:Int, b:Int): Int = Math.pow(a, b).toInt
  override def times(a:Int, b:Int): Int = a * b
  def zero: Int = 0

  override def fromInt(n: Int): Int = n

  def quot(a:Int, b:Int) = a / b
  def mod(a:Int, b:Int) = a % b
  def gcd(a:Int, b:Int): Int = math.gcd(a, b).toInt
}

// Not included in Instances trait.
trait IntIsNRoot extends NRoot[Int] {
  def nroot(x: Int, n: Int): Int = {
    def findnroot(prev: Int, add: Int): Int = {
      val next = prev | add
      val e = Math.pow(next, n)

      if (e == x || add == 0) {
        next
      } else if (e <= 0 || e > x) {
        findnroot(prev, add >> 1)
      } else {
        findnroot(next, add >> 1)
      }
    }

    findnroot(0, 1 << ((33 - n) / n))
  }

  def log(a:Int) = Math.log(a.toDouble).toInt
  def fpow(a:Int, b:Int) = Math.pow(a, b).toInt
}

trait IntOrder extends Order[Int] {
  override def eqv(x: Int, y: Int) = x == y
  override def neqv(x: Int, y: Int) = x != y
  override def gt(x: Int, y: Int) = x > y
  override def gteqv(x: Int, y: Int) = x >= y
  override def lt(x: Int, y: Int) = x < y
  override def lteqv(x: Int, y: Int) = x <= y
  def compare(x: Int, y: Int) = if (x < y) -1 else if (x == y) 0 else 1
}

trait IntIsSigned extends Signed[Int] {
  def signum(a: Int): Int = java.lang.Integer.signum(a)
  def abs(a: Int): Int = if (a < 0) -a else a
}

trait IntIsReal extends IsIntegral[Int] with IntOrder with IntIsSigned {
  def toDouble(n: Int): Double = n.toDouble
}

@SerialVersionUID(0L)
class IntIsBitString extends BitString[Int] with Serializable {
  def one: Int = -1
  def zero: Int = 0
  def and(a: Int, b: Int): Int = a & b
  def or(a: Int, b: Int): Int = a | b
  def complement(a: Int): Int = ~a
  override def xor(a: Int, b: Int): Int = a ^ b

  def signed: Boolean = true
  def width: Int = 32
  def toHexString(n: Int): String = Integer.toHexString(n)

  def bitCount(n: Int): Int = Integer.bitCount(n)
  def highestOneBit(n: Int): Int = Integer.highestOneBit(n)
  def lowestOneBit(n: Int): Int = Integer.lowestOneBit(n)
  def numberOfLeadingZeros(n: Int): Int = Integer.numberOfLeadingZeros(n)
  def numberOfTrailingZeros(n: Int): Int = Integer.numberOfTrailingZeros(n)

  def leftShift(n: Int, i: Int): Int = n << i
  def rightShift(n: Int, i: Int): Int = n >>> i
  def signedRightShift(n: Int, i: Int): Int = n >> i
  def rotateLeft(n: Int, i: Int): Int = Integer.rotateLeft(n, i)
  def rotateRight(n: Int, i: Int): Int = Integer.rotateRight(n, i)
}

@SerialVersionUID(0L)
class IntAlgebra extends IntIsEuclideanRing with IntIsNRoot with IntIsReal with Serializable

trait IntInstances {
  implicit final val IntBitString = new IntIsBitString
  implicit final val IntAlgebra = new IntAlgebra
}

///////////////////////////////////////////////////////////////////////////

//Double implementation

trait DoubleIsField extends Field[Double] {
  override def minus(a:Double, b:Double): Double = a - b
  def negate(a:Double): Double = -a
  def one: Double = 1.0
  def plus(a:Double, b:Double): Double = a + b
 //TODO: take a look at this override and pow method in Rig
  //override def  pow(a:Double, b:Int): Double = Math.pow(a, b)
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
      val tzShared = math.min(tz0, tz1 + exp1 - exp0)
      val n = math.gcd(val0 >>> tz0, val1 >>> tz1) << tzShared

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
  def round(a:Double): Double = math.round(a)
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
      val tzShared = math.min(tz0, tz1 + exp1 - exp0)
      val n = math.gcd(val0 >>> tz0, val1 >>> tz1).toInt << tzShared

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
      val bVal = value(bBits)/*
//////////////////////////////////////////////////////////////////////////////////////

//AdditiveSemigroupOps for infering + operation on generic values
final class AdditiveSemigroupOps[A](lhs:A)(implicit ev:AdditiveSemigroup[A]) {
  def +(rhs:A): A = macro Ops.binop[A, A]
}
*/


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
  def round(a:Float): Float = math.round(a)
  def isWhole(a:Float) = a % 1.0 == 0.0
}

@SerialVersionUID(0L)
class FloatAlgebra extends FloatIsField with FloatIsNRoot with FloatIsTrig with FloatIsReal with Serializable

trait FloatInstances {
  implicit final val FloatAlgebra = new FloatAlgebra
}

///////////////////////////////////////////////////////////////////////////////////

// Convertable implementation



trait ConvertableTo[@miniboxed A] {
  def fromByte(n: Byte): A
  def fromShort(n: Short): A
  def fromInt(n: Int): A
  def fromLong(n: Long): A
  def fromFloat(n: Float): A
  def fromDouble(n: Double): A
 /* def fromBigInt(n: BigInt): A
  def fromBigDecimal(n: BigDecimal): A
  def fromRational(n: Rational): A

  def fromType[B: ConvertableFrom](b: B): A*/
}

 trait ConvertableToByte extends ConvertableTo[Byte] {
  def fromByte(a: Byte): Byte = a
  def fromShort(a: Short): Byte = a.toByte
  def fromInt(a: Int): Byte = a.toByte
  def fromLong(a: Long): Byte = a.toByte
  def fromFloat(a: Float): Byte = a.toByte
  def fromDouble(a: Double): Byte = a.toByte
  def fromBigInt(a: BigInt): Byte = a.toByte
  /* def fromBigDecimal(a: BigDecimal): Byte = a.toByte
  def fromRational(a: Rational): Byte = a.toBigInt.toByte

  def fromType[B: ConvertableFrom](b: B): Byte = ConvertableFrom[B].toByte(b)*/
}

 trait ConvertableToShort extends ConvertableTo[Short] {
  def fromByte(a: Byte): Short = a.toShort
  def fromShort(a: Short): Short = a
  def fromInt(a: Int): Short = a.toShort
  def fromLong(a: Long): Short = a.toShort
  def fromFloat(a: Float): Short = a.toShort
  def fromDouble(a: Double): Short = a.toShort
  def fromBigInt(a: BigInt): Short = a.toShort
   /* def fromBigDecimal(a: BigDecimal): Short = a.toShort
  def fromRational(a: Rational): Short = a.toBigInt.toShort
  def fromType[B: ConvertableFrom](b: B): Short = ConvertableFrom[B].toShort(b)*/
}

 trait ConvertableToInt extends ConvertableTo[Int] {
  def fromByte(a: Byte): Int = a.toInt
  def fromShort(a: Short): Int = a.toInt
  def fromInt(a: Int): Int = a
  def fromLong(a: Long): Int = a.toInt
  def fromFloat(a: Float): Int = a.toInt
  def fromDouble(a: Double): Int = a.toInt
  def fromBigInt(a: BigInt): Int = a.toInt
  /*def fromBigDecimal(a: BigDecimal): Int = a.toInt
  def fromRational(a: Rational): Int = a.toBigInt.toInt

  def fromType[B: ConvertableFrom](b: B): Int = ConvertableFrom[B].toInt(b)*/
}

 trait ConvertableToLong extends ConvertableTo[Long] {
  def fromByte(a: Byte): Long = a.toLong
  def fromShort(a: Short): Long = a.toLong
  def fromInt(a: Int): Long = a.toLong
  def fromLong(a: Long): Long = a
  def fromFloat(a: Float): Long = a.toLong
  def fromDouble(a: Double): Long = a.toLong
  def fromBigInt(a: BigInt): Long = a.toLong
  /*def fromBigDecimal(a: BigDecimal): Long = a.toLong
  def fromRational(a: Rational): Long = a.toBigInt.toLong

  def fromType[B: ConvertableFrom](b: B): Long = ConvertableFrom[B].toLong(b)*/
}

 trait ConvertableToFloat extends ConvertableTo[Float] {
  def fromByte(a: Byte): Float = a.toFloat
  def fromShort(a: Short): Float = a.toFloat
  def fromInt(a: Int): Float = a.toFloat
  def fromLong(a: Long): Float = a.toFloat
  def fromFloat(a: Float): Float = a
  def fromDouble(a: Double): Float = a.toFloat
 /* def fromBigInt(a: BigInt): Float = a.toFloat
  def fromBigDecimal(a: BigDecimal): Float = a.toFloat
  def fromRational(a: Rational): Float = a.toBigDecimal.toFloat

  def fromType[B: ConvertableFrom](b: B): Float = ConvertableFrom[B].toFloat(b)*/
}

 trait ConvertableToDouble extends ConvertableTo[Double] {
  def fromByte(a: Byte): Double = a.toDouble
  def fromShort(a: Short): Double = a.toDouble
  def fromInt(a: Int): Double = a.toDouble
  def fromLong(a: Long): Double = a.toDouble
  def fromFloat(a: Float): Double = a.toDouble
  def fromDouble(a: Double): Double = a
/*  def fromBigInt(a: BigInt): Double = a.toDouble
  def fromBigDecimal(a: BigDecimal): Double = a.toDouble
  def fromRational(a: Rational): Double = a.toBigDecimal.toDouble

  def fromType[B: ConvertableFrom](b: B): Double = ConvertableFrom[B].toDouble(b)*/
}

 trait ConvertableToBigInt extends ConvertableTo[BigInt] {
  def fromByte(a: Byte): BigInt = BigInt(a)
  def fromShort(a: Short): BigInt = BigInt(a)
  def fromInt(a: Int): BigInt = BigInt(a)
  def fromLong(a: Long): BigInt = BigInt(a)
  def fromFloat(a: Float): BigInt = BigInt(a.toLong)
  def fromDouble(a: Double): BigInt = BigInt(a.toLong)
  def fromBigInt(a: BigInt): BigInt = a
 /* def fromBigDecimal(a: BigDecimal): BigInt = a.toBigInt
  def fromRational(a: Rational): BigInt = a.toBigInt

  def fromType[B: ConvertableFrom](b: B): BigInt = ConvertableFrom[B].toBigInt(b)*/
}
/*
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
*/
object ConvertableTo {
  @inline final def apply[A](implicit ev: ConvertableTo[A]) = ev

  implicit final val ConvertableToByte = new ConvertableToByte {}
  implicit final val ConvertableToShort = new ConvertableToShort {}
  implicit final val ConvertableToInt = new ConvertableToInt {}
  implicit final val ConvertableToFloat = new ConvertableToFloat {}
  implicit final val ConvertableToLong = new ConvertableToLong {}
  implicit final val ConvertableToDouble = new ConvertableToDouble {}
  implicit final val ConvertableToBigInt = new ConvertableToBigInt {}
  /*implicit final val ConvertableToBigDecimal = new ConvertableToBigDecimal {}
  implicit final val ConvertableToRational = new ConvertableToRational {}
  implicit final val ConvertableToAlgebraic = new ConvertableToAlgebraic {}
  implicit final val ConvertableToSafeLong = new ConvertableToSafeLong {}
  implicit final val ConvertableToNumber = new ConvertableToNumber {}
  implicit final val ConvertableToNatural = new ConvertableToNatural {}
*/
 
}

trait ConvertableFrom[@miniboxed A] {
  def toByte(a: A): Byte
  def toShort(a: A): Short
  def toInt(a: A): Int
  def toLong(a: A): Long
  def toFloat(a: A): Float
  def toDouble(a: A): Double
 
  def toBigInt(a: A): BigInt
 def toBigDecimal(a: A): BigDecimal
 // def toRational(a: A): Rational
  //def toNumber(a: A): Number

   /* def toType[B: ConvertableTo](a: A): B*/
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
 // def toRational(a: Byte): Rational = Rational(a)
 // def toNumber(a: Byte): Number = Number(a)

   /*def toType[B: ConvertableTo](a: Byte): B = ConvertableTo[B].fromByte(a)*/
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
 // def toRational(a: Short): Rational = Rational(a)
  //def toNumber(a: Short): Number = Number(a)

  /*def toType[B: ConvertableTo](a: Short): B = ConvertableTo[B].fromShort(a)*/
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
 // def toRational(a: Int): Rational = Rational(a)
  //def toNumber(a: Int): Number = Number(a)

  /*def toType[B: ConvertableTo](a: Int): B = ConvertableTo[B].fromInt(a)*/
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
  //def toRational(a: Long): Rational = Rational(a)
  //def toNumber(a: Long): Number = Number(a)

 /*  def toType[B: ConvertableTo](a: Long): B = ConvertableTo[B].fromLong(a)*/
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
  def toBigDecimal(a: Float): BigDecimal = BigDecimal(a.toDouble)
 // def toRational(a: Float): Rational = Rational(a)
 // def toNumber(a: Float): Number = Number(a)

 /* def toType[B: ConvertableTo](a: Float): B = ConvertableTo[B].fromFloat(a)*/
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
  //def toRational(a: Double): Rational = Rational(a)
  //def toNumber(a: Double): Number = Number(a)

  /*def toType[B: ConvertableTo](a: Double): B = ConvertableTo[B].fromDouble(a)*/
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
 // def toRational(a: BigInt): Rational = Rational(a)
  //def toNumber(a: BigInt): Number = Number(a)
  //def toType[B: ConvertableTo](a: BigInt): B = ConvertableTo[B].fromBigInt(a)
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
  //def toRational(a: BigDecimal): Rational = Rational(a)
  //def toNumber(a: BigDecimal): Number = Number(a)

// def toType[B: ConvertableTo](a: BigDecimal): B = ConvertableTo[B].fromBigDecimal(a)
  def toString(a: BigDecimal): String = a.toString
}

/*trait ConvertableFromRational extends ConvertableFrom[Rational] {
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

//  def toType[B: ConvertableTo](a: Rational): B = ConvertableTo[B].fromRational(a)
  def toString(a: Rational): String = a.toString
}
*/
/*trait ConvertableFromAlgebraic extends ConvertableFrom[Algebraic] {
  def toByte(a: Algebraic): Byte = a.toInt.toByte
  def toShort(a: Algebraic): Short = a.toInt.toShort
  def toInt(a: Algebraic): Int = a.toInt
  def toLong(a: Algebraic): Long = a.toLong
  def toFloat(a: Algebraic): Float = a.toDouble.toFloat
  def toDouble(a: Algebraic): Double = a.toDouble
  def toBigInt(a: Algebraic): BigInt = a.toBigInt
  
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
*/
/*trait ConvertableFromSafeLong extends ConvertableFrom[SafeLong] {
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

 // def toType[B: ConvertableTo](a: SafeLong): B = ConvertableTo[B].fromBigInt(a.toBigInt)
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

  //def toType[B: ConvertableTo](a: Number): B = ConvertableTo[B].fromBigInt(a.toBigInt)
  def toString(a: Number): String = a.toString
}*/
/*
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

*/
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
//  implicit final val ConvertableFromRational = new ConvertableFromRational {}
 // implicit final val ConvertableFromAlgebraic = new ConvertableFromAlgebraic {}
//  implicit final val ConvertableFromSafeLong = new ConvertableFromSafeLong {}
  //implicit final val ConvertableFromNumber = new ConvertableFromNumber {}
  //implicit final val ConvertableFromNatural = new ConvertableFromNatural {}

}

//////////////////////////////////////////////////////////////////////////////////////

// Ring, Rig, Rng, Semiring and EuclideanRing


//Ring
trait Ring[@miniboxed A] extends Rig[A] with Rng[A] {
  def fromInt(n: Int): A = additive.sumn(one, n)
}

object Ring {
  @inline final def apply[A](implicit r: Ring[A]): Ring[A] = r
}

trait CRing[@miniboxed A] extends Ring[A] with MultiplicativeCMonoid[A]

object CRing {
  @inline final def apply[A](implicit r: CRing[A]): CRing[A] = r
}


//Rig
trait Rig[@miniboxed A] extends Semiring[A] with AdditiveMonoid[A] with MultiplicativeMonoid[A] {
  override def pow(a:A, n:Int):A =
    if (n >= 0) multiplicative.sumn(a, n)
    else throw new IllegalArgumentException(s"Illegal negative exponent $n to Monoid#pow")
}

object Rig {
  @inline final def apply[A](implicit r:Rig[A]): Rig[A] = r
}

//Rng

trait Rng[@miniboxed A] extends Semiring[A] with AdditiveAbGroup[A]

object Rng {
  @inline final def apply[A](implicit r:Rng[A]):Rng[A] = r
}

//Semiring

trait Semiring[@miniboxed A] extends AdditiveMonoid[A] with MultiplicativeSemigroup[A] {
  def pow(a:A, n:Int):A =
    if (n > 0) multiplicative.sumn(a, n)
    else throw new IllegalArgumentException(s"Illegal non-positive exponent $n to Semiring#pow")
}

object Semiring {
  @inline final def apply[A](implicit r:Semiring[A]):Semiring[A] = r
}

// EuclideanRing


trait EuclideanRing[@miniboxed A] extends CRing[A] {
  def quot(a: A, b: A): A
  def mod(a: A, b: A): A
  def quotmod(a: A, b: A): (A, A) = (quot(a, b), mod(a, b))

  def gcd(a: A, b: A): A
  def lcm(a: A, b: A): A = times(quot(a, gcd(a, b)), b)

  @tailrec protected[this] final def euclid(a: A, b: A)(implicit eq: Eq[A]): A =
    if (eq.eqv(b, zero)) a else euclid(b, mod(a, b))
}

object EuclideanRing {
  @inline final def apply[A](implicit e: EuclideanRing[A]): EuclideanRing[A] = e
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

trait AdditiveSemigroup[@miniboxed A] {
  def additive: Semigroup[A] = new Semigroup[A] {
    def op(x: A, y: A): A = plus(x, y)
  }

  def plus(x: A, y: A): A
}

trait AdditiveCSemigroup[@miniboxed A] extends AdditiveSemigroup[A] {
  override def additive: CSemigroup[A] = new CSemigroup[A] {
    def op(x: A, y: A): A = plus(x, y)
  }
}

trait AdditiveMonoid[@miniboxed A] extends AdditiveSemigroup[A] {
  override def additive: Monoid[A] = new Monoid[A] {
    def id = zero
    def op(x: A, y: A): A = plus(x, y)
  }

  def zero: A
}

trait AdditiveCMonoid[@miniboxed A] extends AdditiveMonoid[A] with AdditiveCSemigroup[A] {
  override def additive: CMonoid[A] = new CMonoid[A] {
    def id = zero
    def op(x: A, y: A): A = plus(x, y)
  }
}

trait AdditiveGroup[@miniboxed A] extends AdditiveMonoid[A] {
  override def additive: Group[A] = new Group[A] {
    def id = zero
    def op(x: A, y: A): A = plus(x, y)
    def inverse(x: A): A = negate(x)
  }

  def negate(x: A): A
  def minus(x: A, y: A): A = plus(x, negate(y))
}

trait AdditiveAbGroup[@miniboxed A] extends AdditiveGroup[A] with AdditiveCMonoid[A] {
  override def additive: AbGroup[A] = new AbGroup[A] {
    def id = zero
    def op(x: A, y: A): A = plus(x, y)
    def inverse(x: A): A = negate(x)
  }
}
//////////////////////////////////////////////////////////////////////////////////////////

//Semigroup


trait Semigroup[@miniboxed A] {
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

trait CSemigroup[@miniboxed A]
    extends Semigroup[A]

object CSemigroup {
  @inline final def apply[A](implicit ev: CSemigroup[A]): CSemigroup[A] = ev
  @inline final def additive[A](implicit A: AdditiveCSemigroup[A]): CSemigroup[A] =  A.additive
  @inline final def multiplicative[A](implicit A: MultiplicativeCSemigroup[A]): CSemigroup[A] = A.multiplicative
}


//////////////////////////////////////////////////////////////////////////////////////////

//Monoid

trait Monoid[@miniboxed A]
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

/*
//////////////////////////////////////////////////////////////////////////////////////

//AdditiveSemigroupOps for infering + operation on generic values
final class AdditiveSemigroupOps[A](lhs:A)(implicit ev:AdditiveSemigroup[A]) {
  def +(rhs:A): A = macro Ops.binop[A, A]
}
*/

}

trait CMonoid[@miniboxed A]
    extends Monoid[A] with CSemigroup[A]

object CMonoid {
  @inline final def apply[A](implicit ev: CMonoid[A]): CMonoid[A] = ev
  @inline final def additive[A](implicit A: AdditiveCMonoid[A]): CMonoid[A] =  A.additive
  @inline final def multiplicative[A](implicit A: MultiplicativeCMonoid[A]): CMonoid[A] = A.multiplicative
}

//////////////////////////////////////////////////////////////////////////////////////////

//Group

trait Group[@miniboxed A]
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


trait AbGroup[@miniboxed A]
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

trait MultiplicativeSemigroup[@miniboxed A] {
  def multiplicative: Semigroup[A] = new Semigroup[A] {
    def op(x: A, y: A): A = times(x, y)
  }

  def times(x: A, y: A): A
}

trait MultiplicativeCSemigroup[@miniboxed A] extends MultiplicativeSemigroup[A] {
  override def multiplicative: CSemigroup[A] = new CSemigroup[A] {
    def op(x: A, y: A): A = times(x, y)
  }
}

trait MultiplicativeMonoid[@miniboxed A] extends MultiplicativeSemigroup[A] {
  override def multiplicative: Monoid[A] = new Monoid[A] {
    def id = one
    def op(x: A, y: A): A = times(x, y)
  }

  def one: A
}

trait MultiplicativeCMonoid[@miniboxed A] extends MultiplicativeMonoid[A] with MultiplicativeCSemigroup[A] {
  override def multiplicative: CMonoid[A] = new CMonoid[A] {
    def id = one
    def op(x: A, y: A): A = times(x, y)
  }
}

trait MultiplicativeGroup[@miniboxed A] extends MultiplicativeMonoid[A] {
  override def multiplicative: Group[A] = new Group[A] {
    def id = one
    def op(x: A, y: A): A = times(x, y)
    def inverse(x: A): A = reciprocal(x)
  }

  def reciprocal(x: A): A = div(one, x)
  def div(x: A, y: A): A
}

trait MultiplicativeAbGroup[@miniboxed A] extends MultiplicativeGroup[A] with MultiplicativeCMonoid[A] {
  override def multiplicative: AbGroup[A] = new AbGroup[A] {
    def id = one
    def op(x: A, y: A): A = times(x, y)
    def inverse(x: A): A = reciprocal(x)
  }
}

/////////////////////////////////////////////////////////////////////////////////////

//NRoot

trait NRoot[@miniboxed A] {
  def nroot(a: A, n: Int): A
  def sqrt(a: A): A = nroot(a, 2)
  def fpow(a:A, b:A): A
}

object NRoot {
  @inline final def apply[@miniboxed A](implicit ev:NRoot[A]) = ev

  private def intSearch(f: Int => Boolean): Int = {
    val ceil = (0 until 32) find (i => !f(1 << i)) getOrElse 33
    if (ceil == 0) {
      0
    } else {
      (0 /: ((ceil - 1) to 0 by -1)) { (x, i) =>
        val y = x | (1 << i)
        if (f(y)) y else x
      }
    }
  }

  private def decDiv(x: BigInt, y: BigInt, r: Int): Stream[BigInt] = {
    val expanded = x * r
    val quot = expanded / y
    val rem = expanded - (quot * y)

    if (rem == 0) {
      Stream.cons(quot, Stream.empty)
    } else {
      Stream.cons(quot, decDiv(rem, y, r))
    }
  }
  private def digitize(x: BigInt, r: Int, prev: List[Int] = Nil): List[Int] =
    if (x == 0) prev else digitize(x / r, r, (x % r).toInt :: prev)
    
  private def undigitize(digits: Seq[Int], r: Int): BigInt =
    (BigInt(0) /: digits)(_ * r + _)

  private val radix = 1000000000

  def nroot(a: BigDecimal, k: Int, ctxt: MathContext): BigDecimal = if (k == 0) {
    BigDecimal(1)
  } else if (a.signum < 0) {
    if (k % 2 == 0) {
      throw new ArithmeticException("%d-root of negative number" format k)
    } else {
      -nroot(-a, k, ctxt)
    }
  } else {
    val underlying = BigInt(a.bigDecimal.unscaledValue.toByteArray)
    val scale = BigInt(10) pow a.scale
    val intPart = digitize(underlying / scale, radix)
    val fracPart = decDiv(underlying % scale, scale, radix) map (_.toInt)
    val leader = if (intPart.size % k == 0) Stream.empty else {
      Stream.fill(k - intPart.size % k)(0)
    }
    val digits = leader ++ intPart.toStream ++ fracPart ++ Stream.continually(0)
    val radixPowK = BigInt(radix) pow k

    val maxSize = (ctxt.getPrecision + 8) / 9 + 2

    def findRoot(digits: Stream[Int], y: BigInt, r: BigInt, i: Int): (Int, BigInt) = {
      val y_ = y * radix
      val a = undigitize(digits take k, radix)
     
      val target = radixPowK * r + a + (y_ pow k)
      val b = intSearch(b => ((y_ + b) pow k) <= target)

      val ny = y_ + b

      if (i == maxSize) {
        (i, ny)
      } else {
        val nr = target - (ny pow k)
        
     
        findRoot(digits drop k, ny, nr, i + 1)
      }
    }

    val (size, unscaled) = findRoot(digits, 0, 0, 1)
    val newscale = (size - (intPart.size + k - 1) / k) * 9
    BigDecimal(unscaled, newscale, ctxt)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////

//IsReal

/**
 * A simple type class for numeric types that are a subset of the reals.
 */
trait IsReal[@miniboxed A] extends Order[A] with Signed[A] {
  def ceil(a: A): A
  def floor(a: A): A
  def round(a: A): A
  def isWhole(a: A): Boolean
  def toDouble(a: A): Double
}

trait IsIntegral[@miniboxed A] extends IsReal[A] {
  def ceil(a: A): A = a
  def floor(a: A): A = a
  def round(a: A): A = a
  def isWhole(a: A): Boolean = true
}

object IsReal {
  def apply[@miniboxed A](implicit A: IsReal[A]): IsReal[A] = A
}

//////////////////////////////////////////////////////////////////////////////////////////////

//Order

trait Order[@miniboxed A] extends Eq[A] {
  self =>

  def eqv(x: A, y: A): Boolean = compare(x, y) == 0
  def gt(x: A, y: A): Boolean = compare(x, y) > 0
  def lt(x: A, y: A): Boolean = compare(x, y) < 0
  def gteqv(x: A, y: A): Boolean = compare(x, y) >= 0
  def lteqv(x: A, y: A): Boolean = compare(x, y) <= 0

  def min(x: A, y: A): A = if (lt(x, y)) x else y
  def max(x: A, y: A): A = if (gt(x, y)) x else y
  def compare(x: A, y: A): Int

  override def on[@miniboxed B](f: B => A): Order[B] = new MappedOrder(this)(f)

  def reverse: Order[A] = new ReversedOrder(this)
}

private class MappedOrder[@miniboxed A, @miniboxed B](order: Order[B])(f: A => B) extends Order[A] {
  def compare(x: A, y: A) = order.compare(f(x), f(y))
}

private class ReversedOrder[@miniboxed A](order: Order[A]) extends Order[A] {
  def compare(x: A, y: A) = order.compare(y, x)
}

object Order {
  @inline final def apply[A](implicit o: Order[A]) = o

  def by[@miniboxed A, @miniboxed B](f: A => B)(implicit o: Order[B]): Order[A] = o.on(f)

  def from[@miniboxed A](f: (A, A) => Int): Order[A] = new Order[A] {
    def compare(x: A, y: A) = f(x, y)
  }

  implicit def ordering[A](implicit o: Order[A]) = new Ordering[A] {
    def compare(x: A, y: A) = o.compare(x, y)
  }
}



///////////////////////////////////////////////////////////////////////////////////////////////

//Signed

trait Signed[@miniboxed A] {
  def sign(a: A): Sign = Sign(signum(a))  
  
  def signum(a: A): Int

  def abs(a: A): A

  def isZero(a: A): Boolean = signum(a) == 0
}

object Signed {
  implicit def orderedRingIsSigned[A: Order: Ring]: Signed[A] = new OrderedRingIsSigned[A]

  def apply[A](implicit s: Signed[A]): Signed[A] = s
}

private class OrderedRingIsSigned[A](implicit o: Order[A], r: Ring[A]) extends Signed[A] {
  def signum(a: A) = o.compare(a, r.zero)
  def abs(a: A) = if (signum(a) < 0) r.negate(a) else a
}
//////////////////////////////////////////////////////////////////////////////////////////////

//Sign

sealed abstract class Sign(val toInt: Int) {
  import Sign._

  def unary_-(): Sign = this match {
    case Positive => Negative
    case Negative => Positive
    case Zero => Zero
  }

  def *(that: Sign): Sign = Sign(this.toInt * that.toInt)

  def **(that: Int): Sign = Sign(math.pow(this.toInt, that).toInt)
}

object Sign {
  case object Zero extends Sign(0)
  case object Positive extends Sign(1)
  case object Negative extends Sign(-1)

  implicit def sign2int(s: Sign): Int = s.toInt
  implicit def apply(i: Int): Sign = 
    if (i == 0) Zero else if (i > 0) Positive else Negative

  implicit final val SignAlgebra = new SignAlgebra {}

  trait SignAlgebra extends Group[Sign] with Signed[Sign] with Order[Sign] {
    def id: Sign = Zero
    def op(a: Sign, b: Sign): Sign = Sign(a.toInt * b.toInt)
    def inverse(a: Sign): Sign = Sign(-a.toInt)
    override def sign(a: Sign): Sign = a
    def signum(a: Sign): Int = a.toInt
    def abs(a: Sign): Sign = if (a == Negative) Positive else a
    def compare(x: Sign, y: Sign): Int = x.toInt - y.toInt
  }
}

//////////////////////////////////////////////////////////////////////////////////////////////

//Eq

trait Eq[@miniboxed A] {
  def eqv(x:A, y:A): Boolean

  def neqv(x:A, y:A): Boolean = !eqv(x, y)

  def on[@miniboxed B](f:B => A): Eq[B] = new MappedEq(this)(f)
}

private class MappedEq[@miniboxed A, @miniboxed B](eq: Eq[B])(f: A => B) extends Eq[A] {
  def eqv(x: A, y: A): Boolean = eq.eqv(f(x), f(x))
}

object Eq {
  def apply[A](implicit e:Eq[A]):Eq[A] = e

  def by[@miniboxed A, @miniboxed B](f:A => B)(implicit e:Eq[B]): Eq[A] = new MappedEq(e)(f)
}


////////////////////////////////////////////////////////////////////////////////////////

// Math 

package object math {
  
   /**
   * abs
   */
  final def abs(n: Byte): Byte = Math.abs(n).toByte
  final def abs(n: Short): Short = Math.abs(n).toShort
  final def abs(n: Int): Int = Math.abs(n)
  final def abs(n: Long): Long = Math.abs(n)
  final def abs(n: Float): Float = Math.abs(n)
  final def abs(n: Double): Double = Math.abs(n)
  final def abs[A](a: A)(implicit ev: Signed[A]): A = ev.abs(a)
  
  /**
   * ceil
   */
  final def ceil(n: Float): Float = Math.ceil(n).toFloat
  final def ceil(n: Double): Double = Math.ceil(n)
 // final def ceil(n: BigDecimal): BigDecimal = n.setScale(0, CEILING)
  final def ceil[A](a: A)(implicit ev: IsReal[A]): A = ev.ceil(a)


  /**
   * min
   */
  final def min(x: Byte, y: Byte): Byte = Math.min(x, y).toByte
  final def min(x: Short, y: Short): Short = Math.min(x, y).toShort
  final def min(x: Int, y: Int): Int = Math.min(x, y)
  final def min(x: Long, y: Long): Long = Math.min(x, y)
  final def min(x: Float, y: Float): Float = Math.min(x, y)
  final def min(x: Double, y: Double): Double = Math.min(x, y)
  final def min[A](x: A, y: A)(implicit ev: Order[A]) = ev.min(x, y)

  /**
   * max
   */
  final def max(x: Byte, y: Byte): Byte = Math.max(x, y).toByte
  final def max(x: Short, y: Short): Short = Math.max(x, y).toShort
  final def max(x: Int, y: Int): Int = Math.max(x, y)
  final def max(x: Long, y: Long): Long = Math.max(x, y)
  final def max(x: Float, y: Float): Float = Math.max(x, y)
  final def max(x: Double, y: Double): Double = Math.max(x, y)
  final def max[A](x: A, y: A)(implicit ev: Order[A]) = ev.max(x, y)
  
   /**
   * round
   */
  final def round(a: Float): Float =
    if (Math.abs(a) >= 16777216.0F) a else Math.round(a).toFloat
  final def round(a: Double): Double =
    if (Math.abs(a) >= 4503599627370496.0) a else Math.round(a).toDouble
  final def round[A](a: A)(implicit ev: IsReal[A]): A = ev.round(a)
  
   /**
   * gcd
   */
  final def gcd(_x: Long, _y: Long): Long = {
    if (_x == 0L) return Math.abs(_y)
    if (_y == 0L) return Math.abs(_x)
  
    var x = _x
    var xz = numberOfTrailingZeros(x)
    x = Math.abs(x >> xz)
  
    var y = _y
    var yz = numberOfTrailingZeros(y)
    y = Math.abs(y >> yz)

    while (x != y) {
      if (x > y) {
        x -= y
        x >>= numberOfTrailingZeros(x)
      } else {
        y -= x
        y >>= numberOfTrailingZeros(y)
      }
    }

    if (xz < yz) x << xz else x << yz
  }

  final def gcd(a: BigInt, b: BigInt): BigInt = a.gcd(b)
  final def gcd[A](x: A, y: A)(implicit ev: EuclideanRing[A]): A = ev.gcd(x, y)
  final def gcd[A](xs: Seq[A])(implicit ev: EuclideanRing[A]): A =
    xs.foldLeft(ev.zero) { (x, y) => gcd(y, x) }
  final def gcd[A](x: A, y: A, z: A, rest: A*)(implicit ev: EuclideanRing[A]): A =
    gcd(gcd(gcd(x, y), z), gcd(rest))
 
    /**
   * exp
   */
  final def exp(n: Double): Double = Math.exp(n)

  /*final def exp(k: Int, precision: Int): BigDecimal = {
    val mc = new MathContext(precision + 1, RoundingMode.HALF_UP)
    var i = 2
    var num = BigInt(2)
    var denom = BigInt(1)

    val limit = BigInt(10).pow(precision)
    while (denom < limit) {
      denom = denom * i
      num = num * i + BigInt(1)
      i += 1
    }
    val sum = BigDecimal(num, mc) / BigDecimal(denom, mc)
    sum.setScale(precision - sum.precision + sum.scale, FLOOR).pow(k)
  }*/
  final def exp[A](a: A)(implicit t: Trig[A]): A = t.exp(a)

  /**
   * log
   */
  final def log(n: Double): Double = Math.log(n)

  final def log[A](a: A)(implicit t: Trig[A]): A = t.log(a)
  /**
   * pow
   */
  
  final def pow(base: Long, exponent: Long): Long = {
    @tailrec def longPow(t: Long, b: Long, e: Long): Long =
      if (e == 0L) t
      else if ((e & 1) == 1) longPow(t * b, b * b, e >> 1L)
      else longPow(t, b * b, e >> 1L)

    if (exponent < 0L) {
      if(base == 0L) sys.error("zero can't be raised to negative power")
      else if (base == 1L) 1L
      else if (base == -1L) if ((exponent & 1L) == 0L) -1L else 1L
      else 0L
    } else {
      longPow(1L, base, exponent)
    }
  }

  final def pow(base: Double, exponent: Double) = Math.pow(base, exponent)
  
  //BigDecimal pow
  implicit object myBigDecimal extends Trig[BigDecimal]{
   def acos(a: BigDecimal): BigDecimal =  Math.acos(a.toDouble)
   def asin(a: BigDecimal): BigDecimal =  Math.acos(a.toDouble)
   def atan(a: BigDecimal): BigDecimal =  Math.acos(a.toDouble)
   def atan2(y: BigDecimal,x: BigDecimal): BigDecimal =  Math.atan2(x.toDouble, y.toDouble)
   def cos(a: BigDecimal): BigDecimal = Math.cos(a.toDouble)
   def cosh(x: BigDecimal): BigDecimal =  Math.cosh(x.toDouble)
   def e: BigDecimal = 1
   def exp(a: BigDecimal): BigDecimal =  Math.exp(a.toDouble)
   def expm1(a: BigDecimal): BigDecimal =  Math.expm1(a.toDouble)
   def log(a: BigDecimal): BigDecimal = Math.log(a.toDouble)
   def log1p(a: BigDecimal): BigDecimal = Math.log1p(a.toDouble)
   def pi: BigDecimal = 3.14
   def sin(a: BigDecimal): BigDecimal = Math.sin(a.toDouble)
   def sinh(x: BigDecimal): BigDecimal = Math.sinh(x.toDouble)
   def tan(a: BigDecimal): BigDecimal = Math.tan(a.toDouble)
   def tanh(x: BigDecimal): BigDecimal = Math.tanh(x.toDouble)
   def toDegrees(a: BigDecimal): BigDecimal = Math.toDegrees(a.toDouble)
   def toRadians(a: BigDecimal): BigDecimal = Math.toRadians(a.toDouble)

  }
  final def pow(base: BigDecimal, exponent: BigDecimal) =
    if (exponent.abs <= 99999999 && exponent.isWhole)
      base.pow(exponent.toInt)
    else
      exp(log(base) * exponent)

  /**
   * sqrt
   */
  final def sqrt(x: Double): Double = Math.sqrt(x)
  final def sqrt[A](a: A)(implicit ev: NRoot[A]): A = ev.sqrt(a)

    
  
}

////////////////////////////////////////////////////////////////////////////////////////

//Field


trait Field[@miniboxed A] extends EuclideanRing[A] with MultiplicativeAbGroup[A] {

  /**
   * This is implemented in terms of basic Field ops. However, this is
   * probably significantly less efficient than can be done with a specific
   * type. So, it is recommended that this method is overriden.
   *
   * This is possible because a Double is a rational number.
   */
  def fromDouble(a: Double): A = if (a == 0.0) {
    fromInt(0)
  } else {
    require(!isInfinite(a) && !isNaN(a),
            "Double must be representable as a fraction.")

    val bits = doubleToLongBits(a)
    val m = bits & 0x000FFFFFFFFFFFFFL | 0x0010000000000000L
    val zeros = numberOfTrailingZeros(m)
    val value = m >>> zeros
    val exp = ((bits >> 52) & 0x7FF).toInt - 1075 + zeros // 1023 + 52

    val high = times(fromInt((value >>> 30).toInt), fromInt(1 << 30))
    val low = fromInt((value & 0x3FFFFFFF).toInt)
    val num = plus(high, low)
    val unsigned = if (exp > 0) {
      times(num, pow(fromInt(2), exp))
    } else if (exp < 0) {
      div(num, pow(fromInt(2), -exp))
    } else {
      num
    }

    if (a < 0) negate(unsigned) else unsigned
  }
}

object Field {
  @inline final def apply[A](implicit f: Field[A]): Field[A] = f
}


/////////////////////////////////////////////////////////////////////////////////////////

//Trig

trait Trig[@miniboxed A] {
  def e: A
  def pi: A

  def exp(a: A): A
  def expm1(a: A): A
  def log(a:A): A
  def log1p(a: A): A

  def sin(a: A): A
  def cos(a: A): A
  def tan(a: A): A

  def asin(a: A): A
  def acos(a: A): A
  def atan(a: A): A
  def atan2(y: A, x: A): A

  def sinh(x: A): A
  def cosh(x: A): A
  def tanh(x: A): A

  def toRadians(a: A): A
  def toDegrees(a: A): A
}

object Trig {
  @inline final def apply[A](implicit t: Trig[A]) = t
}
///////////////////////////////////////////////////////////////////////////////////////

//Complex and FastComplex

final case class Complex[@miniboxed T](real: T, imag: T)
   
object FastComplex {
  final def apply(real: Float, imag: Float) = encode(real, imag)
  final def apply(real: Double, imag: Double) = encode(real.toFloat, imag.toFloat)
  
  // encode two floats representing a complex number
  @inline final def encode(real: Float, imag: Float): Long = {
    (bits(imag).toLong << 32) + bits(real).toLong
  }
  @inline final def bits(n: Float): Int = java.lang.Float.floatToRawIntBits(n)
  
}


///////////////////////////////////////////////////////////////////////////////////////

// ArraySupport
object ArraySupport {
 
  
  def plus[@miniboxed A: ClassTag: AdditiveMonoid](x: Array[A], y: Array[A]): Array[A] = {
   
    val z = new Array[A](math.max(x.length, y.length))
    var i = 0
    while (i < x.length && i < y.length) { z(i) = implicitly[AdditiveMonoid[A]].plus(x(i), y(i)); i += 1 }
    while (i < x.length) { z(i) = x(i); i += 1 }
    while (i < y.length) { z(i) = y(i); i += 1 }
    z
  }
  
  def eqv[@miniboxed A: Eq](x: Array[A], y: Array[A]): Boolean = {
    var i = 0
    if (x.length != y.length) return false
    while (i < x.length && i < y.length && x(i) == y(i)) i += 1
    i == x.length
  }
  
   def concat[@miniboxed A: ClassTag](x: Array[A], y: Array[A]): Array[A] = {
    val z = new Array[A](x.length + y.length)
    System.arraycopy(x, 0, z, 0, x.length)
    System.arraycopy(y, 0, z, x.length, y.length)
    z
  }
   
  def compare[@miniboxed A: Order](x: Array[A], y: Array[A]): Int = {
    var i = 0
    while (i < x.length && i < y.length) {
      val cmp =  implicitly[Order[A]].compare(x(i), y(i))
      if (cmp != 0) return cmp
      i += 1
    }
    x.length - y.length
  }
  def negate[@miniboxed A: ClassTag: Ring](x: Array[A]): Array[A] = {
    val y = new Array[A](x.length)
    var i = 0
    while (i < x.length) {
      y(i) = implicitly[Ring[A]].negate(x(i))
      i += 1
    }
    y
  }
     
  def timesl[@miniboxed A: ClassTag: MultiplicativeSemigroup](r: A, x: Array[A]): Array[A] = {
    val y = new Array[A](x.length)
    var i = 0
    while (i < y.length) { y(i) =implicitly[MultiplicativeSemigroup[A]].times(r, x(i)); i += 1 }
    y
  }
}



