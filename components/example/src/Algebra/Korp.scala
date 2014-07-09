package Algebra

import spire.algebra._
import scala.{ specialized => spec }
import spire.math._
import spire._

object ax{
val x = spire.math.pow(1,1)

implicit def apply(x: Number): Rational = x match {
    case RationalNumber(n) => apply(n)
    case IntNumber(n) => apply(n)
    case FloatNumber(n) => apply(n)
    case DecimalNumber(n) => apply(n)
  }
}
// Represents an abelian group whose elements can have an order
trait Korp[@spec(Byte, Short, Int, Long, Float, Double) T] extends Order[T] with AdditiveAbGroup[T] with MultiplicativeAbGroup[T]

