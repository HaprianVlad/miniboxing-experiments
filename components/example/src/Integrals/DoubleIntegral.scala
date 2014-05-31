
package Integrals

object DoubleIntegral{
  def leftRect(f:Double=>Double, a:Double, b:Double)=f(a)
                                                  //> leftRect: (f: Double => Double, a: Double, b: Double)Double
  def midRect(f:Double=>Double, a:Double, b:Double)=f((a+b)/2)
                                                  //> midRect: (f: Double => Double, a: Double, b: Double)Double
  def rightRect(f:Double=>Double, a:Double, b:Double)=f(b)
                                                  //> rightRect: (f: Double => Double, a: Double, b: Double)Double
  def trapezoid(f:Double=>Double, a:Double, b:Double)=(f(a)+f(b))/2
                                                  //> trapezoid: (f: Double => Double, a: Double, b: Double)Double
  def simpson(f:Double=>Double, a:Double, b:Double)=(f(a)+4*f((a+b)/2)+f(b))/6;
                                                  //> simpson: (f: Double => Double, a: Double, b: Double)Double
 
  def fn1(x:Double)=x*x*x                         //> fn1: (x: Double)Double
  def fn2(x:Double)=1/x                           //> fn2: (x: Double)Double
  def fn3(x:Double)=x                             //> fn3: (x: Double)Double
 
  type Method = (Double=>Double, Double, Double) => Double
  def integrate(f:Double=>Double, a:Double, b:Double, steps:Double, m:Method)={
    val delta:Double=(b-a)/steps
    delta*(a until b by delta).foldLeft(0.0)((s,x) => s+m(f, x, x+delta))
  }                                               //> integrate: (f: Double => Double, a: Double, b: Double, steps: Double, m: (Do
                                                  //| uble => Double, Double, Double) => Double)Double

  def print(f:Double=>Double, a:Double, b:Double, steps:Double)={
    println("rectangular left   : %f".format(integrate(f, a, b, steps, leftRect)))
    println("rectangular middle : %f".format(integrate(f, a, b, steps, midRect)))
    println("rectangular right  : %f".format(integrate(f, a, b, steps, rightRect)))
    println("trapezoid          : %f".format(integrate(f, a, b, steps, trapezoid)))
    println("simpson            : %f".format(integrate(f, a, b, steps, simpson)))
  }                                               //> print: (f: Double => Double, a: Double, b: Double, steps: Double)Unit
 
  def main(args: Array[String]): Unit = {
    print(fn1, 0, 1, 100)
    println("------")
    print(fn2, 1, 100, 1000)
    println("------")
    print(fn3, 0, 5000, 5000000)
    println("------")
    print(fn3, 0, 6000, 6000000)
  }                                               //> main: (args: Array[String])Unit
}