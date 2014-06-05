package Algebra

import spire.algebra._
import scala.{ specialized => spec }

// Represents an abelian group whose elements can have an order
trait Korp[@spec(Byte, Short, Int, Long, Float, Double) T] extends Order[T] with AdditiveAbGroup[T] with MultiplicativeAbGroup[T]
