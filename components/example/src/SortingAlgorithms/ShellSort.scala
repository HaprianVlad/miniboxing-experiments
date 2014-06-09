package SortingAlgorithms

import scala.collection.mutable.ArraySeq
import spire.algebra._
import spire.math._
import spire.implicits._


object ShellSort {
   def sort[T](vals: ArraySeq[T])(implicit o: Order[T], g: Group[T]) {
    //Creating our gap sequence
    val gaps = Array(701, 301, 132, 57, 23, 10, 4, 1)
    for (gap <- gaps) {
      for (i <- gap until vals.length) {
        val temp = vals(i)
        var j = i
        while (j >= gap && temp < vals(j-gap) ) {
          vals(j) = vals(j-gap)
          j -= gap
        }
        vals(j) = temp;
      }
    }
  }
}