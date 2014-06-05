package SortingAlgorithms

import scala.collection.mutable.ArraySeq
import spire.algebra._
import spire.math._
import spire.implicits._
import Algebra.Korp


object HeapSort {
  def sort[T](vals: ArraySeq[T])(implicit o: Order[T], g: Group[T]) {
    var heap = new MaxHeap[T]
    heap.set(vals)
    for (i <- vals.length - 1 to 0 by -1) {
      vals(i) = heap.deleteMax
    }
  }
}

class MaxHeap[T](implicit o: Order[T], g: Group[T]) {
  var _vals: ArraySeq[T] = _

  def set(vals: ArraySeq[T]) {
    _vals = new ArraySeq[T](1)
    _vals(0) = implicitly[Group[T]].id


    _vals ++= vals.clone

    // Shift down starting at the last internal node.
    for (i <- (_vals.length-1)/2 until 0 by -1) {
      shiftDown(i)
    }
  }

  def deleteMax(): T = {
    val size = _vals.length - 1
    if (size == 0) {
      throw new RuntimeException
    }

    val maxItem = _vals(1)
    _vals(1) = _vals(size)
    shiftDown(1)

    return maxItem
  }

  private def shiftDown(position : Int) {
    var positionVar = position
    val tmp = _vals(positionVar)
    val size = _vals.length - 1

    while (positionVar * 2 <= size) {
      var child = positionVar * 2
      if (child != size && _vals(child+1) > _vals(child) ) {
        child += 1
      }
      if (_vals(child) > tmp) {
        _vals(positionVar) = _vals(child)
      } else {
        _vals(positionVar) = tmp
        return;
      }
      positionVar = child
    }
    _vals(positionVar) = tmp
  }
}
