package SortingAlgorithms

import scala.collection.mutable.ArraySeq
import SortingAlgorithms._
import java.util.Random
import scala.language.postfixOps
import spire.implicits._
import spire.random.immutable.Generator
import spire.algebra.Group
import spire.algebra.Order

object SortingTest {
	def main(args: Array[String]): Unit = {
			
			val r = new Random()
			val doubleArray = generateArray(10,ArraySeq[Double](10),r)
			val heapSortArray = doubleArray
			val quickSortArray = doubleArray
			val insertionSortArray = doubleArray
			implicit object myGroup extends Group[Double]{
			  def id = 0
			  def inverse(a:Double):Double = -a
			  def op(x:Double,y:Double):Double = x+y
			}
			implicit object myOrder extends Order[Double]{
			  def compare(x:Double,y:Double):Int = 
    	  	  if(x<y) -1 
    	  	  else if (x > y) 1
    	  	  else 0
			}
			println("Unsorted array:")
			println(doubleArray)
			println("----------------------------------------")
			println("Sorted array with HeapSort")
			time{HeapSort.sort(heapSortArray)(myOrder,myGroup)}
			println("----------------------------------------")
			println("Sorted array with QuickSort")
			time{QuickSort.sort(quickSortArray)(myOrder,myGroup)}
			println("----------------------------------------")
			println("Sorted array with InsertionSort")
			time{InsertionSort.sort(insertionSortArray)(myOrder,myGroup)}
			println("----------------------------------------")
			println("Sorted array:")
			println(insertionSortArray) 

	}


	private def generateArray(size:Int,array:ArraySeq[Double],r:Random):ArraySeq[Double]={
			if(size == 0)
				array
			else generateArray(size-1,(r.nextDouble()*100) +: array,r)
	}

	private def time[R](block: => R): R = {
			val t0 = System.nanoTime()
			val result = block    // call-by-name
			val t1 = System.nanoTime()
			println("Elapsed time: " + (t1 - t0) + "ns")
			result
	}

}