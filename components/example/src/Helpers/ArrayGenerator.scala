package Helpers

import scala.collection.mutable.ArraySeq
import org.scalacheck._
import Arbitrary._
import Gen._
import spire.algebra._
import spire.math._
import spire.implicits._

class ArrayGenerator[T](implicit o: Order[T], g: Group[T]) {

  def generateArray(size:Int,array:ArraySeq[T],generator:Gen[T]):ArraySeq[T]={
			if(size == 0)
				array
			else {
			  val generatedValue :T =  generator.sample match{
			    case Some(x) => x
			    case None => implicitly[Group[T]].id

			  }
			  generateArray(size-1,(generatedValue) +: array,generator)
			}
	}
  
}