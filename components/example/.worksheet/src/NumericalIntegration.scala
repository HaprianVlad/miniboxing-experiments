object NumericalIntegration {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(87); 
  def leftRect(f:Double=>Double, a:Double, b:Double)=f(a);System.out.println("""leftRect: (f: Double => Double, a: Double, b: Double)Double""");$skip(63); 
  def midRect(f:Double=>Double, a:Double, b:Double)=f((a+b)/2);System.out.println("""midRect: (f: Double => Double, a: Double, b: Double)Double""");$skip(59); 
  def rightRect(f:Double=>Double, a:Double, b:Double)=f(b);System.out.println("""rightRect: (f: Double => Double, a: Double, b: Double)Double""");$skip(68); 
  def trapezoid(f:Double=>Double, a:Double, b:Double)=(f(a)+f(b))/2;System.out.println("""trapezoid: (f: Double => Double, a: Double, b: Double)Double""");$skip(80); 
  def simpson(f:Double=>Double, a:Double, b:Double)=(f(a)+4*f((a+b)/2)+f(b))/6;System.out.println("""simpson: (f: Double => Double, a: Double, b: Double)Double""");$skip(28); ;
 
  def fn1(x:Double)=x*x*x;System.out.println("""fn1: (x: Double)Double""");$skip(24); 
  def fn2(x:Double)=1/x;System.out.println("""fn2: (x: Double)Double""");$skip(22); 
  def fn3(x:Double)=x
 
  type Method = (Double=>Double, Double, Double) => Double;System.out.println("""fn3: (x: Double)Double""");$skip(252); 
  def integrate(f:Double=>Double, a:Double, b:Double, steps:Double, m:Method)={
    val delta:Double=(b-a)/steps
    delta*(a until b by delta).foldLeft(0.0)((s,x) => s+m(f, x, x+delta))
  };System.out.println("""integrate: (f: Double => Double, a: Double, b: Double, steps: Double, m: (Double => Double, Double, Double) => Double)Double""");$skip(487); 
 
  def print(f:Double=>Double, a:Double, b:Double, steps:Double)={
    println("rectangular left   : %f".format(integrate(f, a, b, steps, leftRect)))
    println("rectangular middle : %f".format(integrate(f, a, b, steps, midRect)))
    println("rectangular right  : %f".format(integrate(f, a, b, steps, rightRect)))
    println("trapezoid          : %f".format(integrate(f, a, b, steps, trapezoid)))
    println("simpson            : %f".format(integrate(f, a, b, steps, simpson)))
  };System.out.println("""print: (f: Double => Double, a: Double, b: Double, steps: Double)Unit""");$skip(235); 
 
  def main(args: Array[String]): Unit = {
    print(fn1, 0, 1, 100)
    println("------")
    print(fn2, 1, 100, 1000)
    println("------")
    print(fn3, 0, 5000, 5000000)
    println("------")
    print(fn3, 0, 6000, 6000000)
  };System.out.println("""main: (args: Array[String])Unit""")}
}
