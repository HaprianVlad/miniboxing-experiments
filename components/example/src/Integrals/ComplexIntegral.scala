package Integrals

import spire.implicits._
import spire.math.Complex
import Algebra.Korp


class ComplexIntegral[T:Korp]{
  
  def leftRect(f:T=>T, a:T, b:T)=f(a)
                                                  //> leftRect: (f: Double => Double, a: Double, b: Double)Double
  def midRect(f:T=>T, a:T, b:T)=f((a+b)/2)
                                                  //> midRect: (f: Double => Double, a: Double, b: Double)Double
  def rightRect(f:T=>T, a:T, b:T)=f(b)
                                                  //> rightRect: (f: Double => Double, a: Double, b: Double)Double
   
  type complexMethod  = (T => T ,T, T) => T
  def integrateComplex(f:T=>Complex[T], a:T, b:T, steps:T, m:complexMethod):Complex[T]= {
    val delta=(b-a)/steps
    val re = delta*(a until b by delta).foldLeft(0.0)((s,x) => s+m(x=>f(x).real, x, x+delta))
    val im = delta*(a until b by delta).foldLeft(0.0)((s,x) => s+m(x=>f(x).imag, x, x+delta))
    Complex(re,im)
  }     
  
  def print(f:T=>Complex[T], a:T, b:T, steps:T)={
    println("rectangular left   :"+integrateComplex(f, a, b, steps, leftRect))
    println("rectangular middle :"+integrateComplex(f, a, b, steps, midRect))
    println("rectangular right  :"+integrateComplex(f, a, b, steps, rightRect))
  
  }            
  
  
}