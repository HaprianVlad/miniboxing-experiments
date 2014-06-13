package Algorithms

import spire.algebra._
import spire.math._
import spire.implicits._


object EuclidGCDMiniboxed{
  
  def gcd[@miniboxed T](a: T, b: T)(implicit o: Order[T], g: EuclideanRing[T]) : T =
	if (b == 0) a else gcd(b, a % b)
}