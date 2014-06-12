package Algorithms
import spire.algebra._
import spire.math._
import spire.implicits._

class EuclidGCD[T](implicit o: Order[T], g: EuclideanRing[T]) {
  
  def gcd(a: T, b: T): T =
	if (b == 0) a else gcd(b, a % b)
}