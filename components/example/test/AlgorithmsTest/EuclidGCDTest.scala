package AlgorithmsTest
import spire.algebra._
import spire.math._
import spire.implicits._
import Helpers.TimeProfiler
import Algorithms.EuclidGCD



object EuclidGCDTest {
  
  	def main(args: Array[String]): Unit = {
			
  			object myRingLong extends EuclideanRing[Long]{
			  def id = 0
			  def inverse(a:Long):Long = -a
			  def op(x:Long,y:Long):Long = x+y
			}
			 object myOrderLong extends Order[Long]{
			  def compare(x:Long,y:Long):Int = 
    	  	  if(x<y) -1 
    	  	  else if (x > y) 1
    	  	  else 0
			}
			
			object myRingInt extends EuclideanRing[Int]{
			  def id = 0
			  def inverse(a:Int):Int = -a
			  def op(x:Int,y:Int):Int = x+y
			}
			 object myOrderInt extends Order[Int]{
			  def compare(x:Int,y:Int):Int = 
    	  	  if(x<y) -1 
    	  	  else if (x > y) 1
    	  	  else 0
			}
			val a : Long = 8
			val b : Long = 12 
			val c : Int = 8
			val d : Int = 12 
			println("Spire specialized Int:")
			TimeProfiler.time{EuclidGCD.gcd(c,d)(myOrderInt,myRingInt)}
			println("---------------------------------")
			println("Spire specialized Long:")
			TimeProfiler.time{EuclidGCD.gcd(a,b)(myOrderLong,myRingLong)}
			println("---------------------------------")
			println("Spire miniboxed Int:")
			//TimeProfiler.time{EuclidGCDMiniboxed.gcd(c, d)}
			println("---------------------------------")
			println("Spire miniboxed long:")
			//TimeProfiler.time{EuclidGCDMiniboxed.gcd(a, b)}
			println("---------------------------------")
			println("Manually specialized Int:")
			TimeProfiler.time{gcdInt(c, d)}
			println("---------------------------------")
			println("Manually specialized Long:")
			TimeProfiler.time{gcdLong(a, b)}
			println("---------------------------------")
			
  	}
  	
  	def gcdInt(a: Int, b: Int):Int =
  		if (b == 0) a else gcd(b, a % b)
	def gcdLong(a: Long, b: Long):Long =
		if (b == 0) a else gcd(b, a % b)
  	
}