package Algorithms
import spire.algebra._
import spire.math._
import spire.implicits._
import Helpers.TimeProfiler



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
			val gcdIntSpire = new EuclidGCD(myOrderInt,myRingInt)
			val gcdLongSpire =new EuclidGCD(myOrderLong,myRingLong)
			val gcdIntMiniboxed = new EuclidGCDMiniboxed[Int]()
			val gcdLongMiniboxed= new EuclidGCDMiniboxed[Long]()
			println("Spire specialized Int:")
			TimeProfiler.time{gcdIntSpire.gcd(8, 12)}
			println("---------------------------------")
			println("Spire specialized Long:")
			TimeProfiler.time{gcdLongSpire.gcd(8, 12)}
			println("---------------------------------")
			println("Spire miniboxed Int:")
			TimeProfiler.time{gcdIntMiniboxed.gcd(8, 12)}
			println("---------------------------------")
			println("Spire miniboxed long:")
			TimeProfiler.time{gcdLongMiniboxed.gcd(8, 12)}
			println("---------------------------------")
			println("Manually specialized Int:")
			TimeProfiler.time{gcdInt(8, 12)}
			println("---------------------------------")
			println("Manually specialized Long:")
			TimeProfiler.time{gcdLong(8, 12)}
			println("---------------------------------")
			
  	}
  	
  	def gcdInt(a: Int, b: Int):Int =
  		if (b == 0) a else gcd(b, a % b)
	def gcdLong(a: Long, b: Long):Long =
		if (b == 0) a else gcd(b, a % b)
  	
}