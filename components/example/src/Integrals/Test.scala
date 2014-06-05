package Integrals
import spire.math.Complex
import Algebra.Korp

object Test {
  def main(args: Array[String]): Unit = {
      def fn1(x:Double)=x*x*x
      def fn2(x:Double)=1/x
      def complexFn(x:Double)= Complex(fn1(x),fn2(x))
// TODO: Define Korp[Double] with its operations!
// val integral = new ComplexIntegral[Double]()
// integral.computeAndPrint(complexFn, 2, 5,0, 100)
  }
}