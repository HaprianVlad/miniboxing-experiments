package Algebra

import spire.algebra.Order
import spire.algebra.AbGroup
import scala.{ specialized => spec }

// Represents an abelian group whose elements can have an order 
trait Korp[@spec(Byte, Short, Int, Long, Float, Double) T] extends Order[T] with AbGroup[T]
