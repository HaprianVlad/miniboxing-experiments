package Integrals
import spire.math.Complex
import Algebra.Korp

object Test {
  def main(args: Array[String]): Unit = {
      def fn1(x:Double)=x*x*x
      def fn2(x:Double)=1/x
      def complexFn(x:Double)= Complex(fn1(x),fn2(x))
      
      //Defining Korp[Double] with its operations
      implicit object myKorp extends Korp[Double]{
    	  	def zero = 0
    	  	def one = 1 
    	  	def plus(x:Double,y:Double):Double = x+y
    	  	def negate(x:Double):Double = -x
    	  	def div(x:Double,y:Double):Double = x/y
    	  	def times(x:Double,y:Double):Double = x*y
    	  	def compare(x:Double,y:Double):Int = 
    	  	  if(x<y) -1 
    	  	  else if (x > y) 1
    	  	  else 0
    	  	
      }
      
      val integral = new ComplexIntegral[Double]()
      integral.computeAndPrint(complexFn, 2, 5,0, 100)
  }
}