package Integrals

import spire.implicits._
import spire.math.Complex
import Xalgebra.Korp
import spire.algebra._
import Xalgebra.Korp

class ComplexIntegral[T: Korp]{

  private type complexMethod = (T => T ,T, T) => T

  //> leftRect: (f: Double => Double, a: Double, b: Double)Double
  private def leftRect(f:T => T, a:T, b:T) = f(a)
  
  
  // NOTE: We can't divide by 2, since 2 is not part of the group
  //> midRect: (f: Double => Double, a: Double, b: Double)Double
  //private def midRect(f:T => T, a:T, b:T) = f((a + b)/2)
  
  //> rightRect: (f: Double => Double, a: Double, b: Double)Double
  private def rightRect(f:T => T, a:T, b:T) = f(b)
 
  

  private def integrateComplex(f: T => Complex[T], a: T, b: T, steps: T, zero: T, m:complexMethod): Complex[T]= {
    val delta = (b-a)/steps
    val collection = createList(a,b,delta,List.empty)
    val re = delta * collection.foldLeft(zero)((s,x) => s + m(x=>f(x).real, x, x+delta))
    val im = delta * collection.foldLeft(zero)((s,x) => s+m(x=>f(x).imag, x, x+delta))
    Complex(re,im)
  }

  private def createList(start:T,end:T,step:T,list:List[T]):List[T] = {
    if(start == end)
      list
    else{
      val newStart = start + step
      val newList = start::list
      createList(newStart,end,step,newList)
    }
  }

  def computeAndPrint(f:T=>Complex[T], a:T, b:T, steps:T, zero:T)={
    println("rectangular left :"+integrateComplex(f, a, b, steps,zero, leftRect))
   // println("rectangular middle :"+integrateComplex(f, a, b, steps,zero, midRect))
    println("rectangular right :"+integrateComplex(f, a, b, steps,zero, rightRect))

  }
}