package SortingAlgorithms

import scala.collection.mutable.ArraySeq
import spire.algebra._
import spire.math._
import spire.implicits._

object MergeSort {
 def sort[T](vals: ArraySeq[T])(implicit o: Order[T], g: Group[T]) {
    for (i <- 1 until vals.length) {
      val tempValue= vals(i)
      var j = i
      while (j > 0 && tempValue< vals(j-1) ) {
        vals(j) = vals(j-1)
        j -= 1
      }
      vals(j) = tempValue
    }
  }

  private def mergeSort[T](vals: ArraySeq[T], temp: ArraySeq[T],
                        left: Int, right: Int)(implicit o: Order[T], g: Group[T]) {
    if (left < right) {
      val center = (left + right)/2;
      mergeSort(vals, temp, left, center)
      mergeSort(vals, temp, center+1, right)
      merge(vals, temp, left, center+1, right)
    }
  }

  private def merge[T](vals: ArraySeq[T], temp: ArraySeq[T],
                    leftPos: Int, rightPos: Int, rightLimit: Int)(implicit o: Order[T], g: Group[T]) {
    var leftPosVar = leftPos; var rightPosVar = rightPos;
    val leftLimit = rightPos - 1
    var rightLimitVar = rightLimit
    var tempPos = leftPos
    val numElements = rightLimit - leftPos + 1

    while (leftPosVar <= leftLimit && rightPosVar <= rightLimit) {
      if (vals(leftPosVar) <= vals(rightPosVar)) {
        temp(tempPos) = vals(leftPosVar)
        tempPos += 1; leftPosVar += 1;
      } else {
        temp(tempPos) = vals(rightPosVar)
        tempPos +=1; rightPosVar +=1;
      }
    }

    while (leftPosVar <= leftLimit) {
      temp(tempPos) = vals(leftPosVar)
      tempPos += 1; leftPosVar += 1;
    }

    while (rightPosVar <= rightLimit) {
      temp(tempPos) = vals(rightPosVar)
      tempPos += 1; rightPosVar += 1;
    }

    for (i <- 0 until numElements) {
      vals(rightLimitVar) = temp(rightLimitVar)
      rightLimitVar -= 1
    }
  }
}