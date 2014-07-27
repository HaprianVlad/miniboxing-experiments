package Algebra

import spire.algebra._
import scala.{ specialized => spec }

import spire.math.IntNumber
import spire.algebra._
import spire.std._
import spire.implicits._

object ax{
val x = spire.math.pow(1,1)

var a :Array[Int] = new Array(1)
var b :Array[Int] = new Array(1)

var c = a + b

}
// Represents an abelian group whose elements can have an order
trait Korp[@spec(Byte, Short, Int, Long, Float, Double) T] extends Order[T] with AdditiveAbGroup[T] with MultiplicativeAbGroup[T]

