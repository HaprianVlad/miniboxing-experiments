package SortingAlgorithms

import scala.collection.mutable.ArraySeq
import SortingAlgorithms._
import java.util.Random
import scala.language.postfixOps
import spire.implicits._
import spire.random.immutable.Generator
import spire.algebra.Group
import spire.algebra.Order
import Helpers.TimeProfiler

object SortingLongTest {
	def main(args: Array[String]): Unit = {
			
			val r = new Random()
			val LongArray = generateArray(1000,ArraySeq[Long](1000),r)
			val heapSortArray = LongArray
			val quickSortArray = LongArray
			val insertionSortArray = LongArray
			val mergeSortArray = LongArray
			val shellSortArray = LongArray
		
			implicit object myGroup extends Group[Long]{
			  def id = 0
			  def inverse(a:Long):Long = -a
			  def op(x:Long,y:Long):Long = x+y
			}
			implicit object myOrder extends Order[Long]{
			  def compare(x:Long,y:Long):Int = 
    	  	  if(x<y) -1 
    	  	  else if (x > y) 1
    	  	  else 0
			}
			println("Unsorted array:")
			println(LongArray)
			println("----------------------------------------")
			println("Sorted array with HeapSort")
			TimeProfiler.time{HeapSort.sort(heapSortArray)(myOrder,myGroup)}
			println("----------------------------------------")
			println("Sorted array with QuickSort")
			TimeProfiler.time{QuickSort.sort(quickSortArray)(myOrder,myGroup)}
			println("----------------------------------------")
			println("Sorted array with InsertionSort")
			TimeProfiler.time{InsertionSort.sort(insertionSortArray)(myOrder,myGroup)}
			println("----------------------------------------")
			println("Sorted array with MergeSort")
			TimeProfiler.time{MergeSort.sort(mergeSortArray)(myOrder,myGroup)}
			println("----------------------------------------")
			println("Sorted array with ShellSort")
			TimeProfiler.time{ShellSort.sort(shellSortArray)(myOrder,myGroup)}
			println("----------------------------------------")
			println("Sorted array:")
			println(shellSortArray) 

	}

	private def generateArray(size:Int,array:ArraySeq[Long],r:Random):ArraySeq[Long]={
			if(size == 0)
				array
			else generateArray(size-1,(r.nextLong()*size) +: array,r)
	}

}