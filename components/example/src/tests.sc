import spire.algebra.Field
import spire.algebra.VectorSpace
import spire.implicits._
import spire.math.Complex

object tests {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
	val x : Int = 4                           //> x  : Int = 4
	val y = Vector(1,5,3) + Vector(2,1,-5)    //> y  : scala.collection.immutable.Vector[Int] = Vector(3, 6, -2)
	val z = Complex(2,30)                     //> z  : spire.math.Complex[Int] = (2 + 30i)
	val a = List(Complex(1,2),Complex(2,3),Complex(4,5))
                                                  //> a  : List[spire.math.Complex[Int]] = List((1 + 2i), (2 + 3i), (4 + 5i))
	
	def f : (Double =>Double) = x => Math.cosh(x)
                                                  //> f: => Double => Double
	def g : (Double => Double) = x => Math.sin(x)
                                                  //> g: => Double => Double
	def func : Double=>Complex[Double] = x=>Complex(f(x),g(x))
                                                  //> func: => Double => spire.math.Complex[Double]
	
	func(1)                                   //> res0: spire.math.Complex[Double] = (1.543080634815244 + 0.8414709848078965i)
                                                  //| 
 

}