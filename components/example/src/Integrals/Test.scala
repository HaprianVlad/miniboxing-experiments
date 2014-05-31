package Integrals
import spire.math.Complex

object Test {

  
  def main(args: Array[String]): Unit = {
    
    def fn1(x:Double)=x*x*x                         
    def fn2(x:Double)=1/x                           
    def complexFn(x:Double)= Complex(fn1(x),fn2(x))
    val integral =  ComplexIntegral[Double]
    integral.print(complexFn, 2, 5, 100)
  }
}