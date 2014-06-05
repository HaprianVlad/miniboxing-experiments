package SortingAlgorithms

import scala.collection.mutable.ArraySeq
import spire.algebra._
import spire.math._
import spire.implicits._


object QuickSort {
  def sort[T](vals: ArraySeq[T])(implicit o: Order[T], g: Group[T])  {
    sort(vals, 0, vals.length-1)
  }

  private def sort[T](vals: ArraySeq[T], lo: Int, hi: Int)(implicit o: Order[T], g: Group[T]) {
    var pivotIdx = findPivot(vals, lo, hi)
    swap(vals, pivotIdx, hi)
    val mid = partition(vals, lo, hi-1, vals(hi))
    swap(vals, mid, hi)
    if ((mid-lo) > 1) sort(vals, lo, mid-1)
    if ((hi-mid) > 1) sort(vals, mid+1, hi)
  }

  private def findPivot[T](vals: ArraySeq[T], lo: Int, hi: Int)(implicit o: Order[T], g: Group[T]): Int = {
    return findMiddle(vals, lo, (lo+hi)/2, hi)
  }
  
  private def findMiddle[T](vals: ArraySeq[T], lo: Int, mid: Int, hi: Int)(implicit o: Order[T], g: Group[T]): Int = {
    val loVal = vals(lo); val midVal = vals(mid); val hiVal = vals(hi);
    if (loVal > midVal) {
      if (midVal > hiVal) {
        return mid
      } else if (loVal > hiVal) {
        return hi
      } else {
        return lo
      }
    } else {
      if (loVal > hiVal) {
        return lo
      } else if (midVal > hiVal) {
        return hi
      } else {
        return mid
      }
    }
  }


  private def
  partition[T](vals: ArraySeq[T], left: Int, right: Int, pivot:T)(implicit o: Order[T], g: Group[T]): Int = {
    var leftV = left; var rightV = right;
    while (leftV <= rightV) {
      while (vals(leftV) < pivot) {
        leftV = leftV + 1
      }
      while ((rightV >= leftV) && (vals(rightV) >= pivot)) {
        rightV = rightV - 1
      }
      if (rightV > leftV) {
        swap(vals, leftV, rightV)
      }
    }
    return leftV
  }

  private def swap[T](vals: ArraySeq[T], i: Int, j: Int) {
    val temp = vals(i)
    vals(i) = vals(j)
    vals(j) = temp
  }
}