package Algebra

import spire.algebra._
import scala.{ specialized => spec }
import spire.math._
import spire._
import spire.math.IntNumber

object ax{
val x = spire.math.pow(1,1)

}
// Represents an abelian group whose elements can have an order
trait Korp[@spec(Byte, Short, Int, Long, Float, Double) T] extends Order[T] with AdditiveAbGroup[T] with MultiplicativeAbGroup[T]

