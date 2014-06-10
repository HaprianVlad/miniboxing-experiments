package Helpers

import spire.algebra._
import spire.math._
import spire.implicits._

object PrimitiveGenerator{
  
	def generate[T](implicit o: Order[T], g: Group[T]) :T =  implicitly[Group[T]].id

}