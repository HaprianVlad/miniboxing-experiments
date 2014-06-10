package Helpers

import scala.util.Random
import scala.collection.mutable.ArraySeq
import spire.algebra._
import spire.math._
import spire.implicits._

object ArrayGenerator {
  
  val generator = PrimitiveGenerator
  
  def generateArray[T](size:Int,array:ArraySeq[T])(implicit o: Order[T], g: Group[T]):ArraySeq[T]={
			if(size == 0)
				array
			else generateArray(size-1,(generator.generate) +: array)
	}
  
}