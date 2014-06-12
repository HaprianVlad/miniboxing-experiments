package Algorithms
import spire.algebra._
import spire.math._
import spire.implicits._

object EuclidGCD{
  
  def gcd[T](a: T, b: T)(implicit o: Order[T], g: EuclideanRing[T]): T =
	if (b == 0) a else gcd(b, a % b)
}