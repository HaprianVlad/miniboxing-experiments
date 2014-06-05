package SortingAlgorithms

import scala.collection.mutable.ArraySeq
import spire.algebra._
import spire.math._
import spire.implicits._

object InsertionSort {
  def sort[T](vals: ArraySeq[T])(implicit o: Order[T], g: Group[T])  {
    for (i <- 1 until vals.length) {
      val temp = vals(i)
      var j = i
      while (j > 0 && temp < vals(j-1) ) {
        vals(j) = vals(j-1)
        j -= 1
      }
      vals(j) = temp;
    }
  }
}