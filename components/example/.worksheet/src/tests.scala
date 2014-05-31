import spire.algebra.Field
import spire.algebra.VectorSpace
import spire.implicits._
import spire.math.Complex

object tests {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(170); 
  println("Welcome to the Scala worksheet");$skip(17); 
	val x : Int = 4;System.out.println("""x  : Int = """ + $show(x ));$skip(40); 
	val y = Vector(1,5,3) + Vector(2,1,-5);System.out.println("""y  : scala.collection.immutable.Vector[Int] = """ + $show(y ));$skip(23); 
	val z = Complex(2,30);System.out.println("""z  : spire.math.Complex[Int] = """ + $show(z ));$skip(54); 
	val a = List(Complex(1,2),Complex(2,3),Complex(4,5));System.out.println("""a  : List[spire.math.Complex[Int]] = """ + $show(a ));$skip(49); 
	
	def f : (Double =>Double) = x => Math.cosh(x);System.out.println("""f: => Double => Double""");$skip(47); 
	def g : (Double => Double) = x => Math.sin(x);System.out.println("""g: => Double => Double""");$skip(60); 
	def func : Double=>Complex[Double] = x=>Complex(f(x),g(x));System.out.println("""func: => Double => spire.math.Complex[Double]""");$skip(11); val res$0 = 
	
	func(1);System.out.println("""res0: spire.math.Complex[Double] = """ + $show(res$0))}
 

}
