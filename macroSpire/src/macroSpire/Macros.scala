package macroSpire


import scala.reflect.macros.blackbox.Context
import scala.language.implicitConversions
import scala.language.higherKinds
import scala.language.experimental.macros

/**************************************************************************************/
// Macros Implementation for array + operation

final class AdditiveSemigroupOps[A](lhs:A)(implicit ev:AdditiveSemigroup[A]) {
  def +(rhs:A): A = macro Ops.binop[A, A]
  def +(rhs:Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def +(rhs:Double)(implicit ev1:Field[A]): A = macro Ops.binopWithLift[Double, Field[A], A]
}

final class LiteralIntAdditiveSemigroupOps(val lhs: Int) extends AnyVal {
  def +[A](rhs:A)(implicit ev: Ring[A]): A = ev.plus(ev.fromInt(lhs), rhs)
}

final class LiteralLongAdditiveSemigroupOps(val lhs: Long) extends AnyVal {
  def +[A](rhs:A)(implicit ev:Ring[A], c:ConvertableTo[A]): A = ev.plus(c.fromLong(lhs), rhs)
}

final class LiteralDoubleAdditiveSemigroupOps(val lhs: Double) extends AnyVal {
  def +[A](rhs:A)(implicit ev:Field[A]): A = ev.plus(ev.fromDouble(lhs), rhs)
}

trait Ops{
  def binop[A, R](c:Context)(rhs:c.Expr[A]):c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    c.Expr[R](Apply(Select(ev, findMethodName(c)), List(lhs, rhs.tree)))
  }
  def unpack[T[_], A](c:Context) = {
    import c.universe._
    c.prefix.tree match {
      case Apply(Apply(TypeApply(_, _), List(x)), List(ev)) => (ev, x)
      case t => c.abort(c.enclosingPosition,
        "Cannot extract subject of operator (tree = %s)" format t)
    }
  }
     
  def findMethodName(c:Context) = {
    import c.universe._
    val s = c.macroApplication.symbol.name.toString
     c.universe.TermName(operatorNames.getOrElse(s, s))
    
  }
  
  def binopWithLift[A: c.WeakTypeTag, Ev, R](c: Context)(rhs: c.Expr[A])(ev1: c.Expr[Ev]): c.Expr[R] = {
    import c.universe._
    val (ev0, lhs) = unpack(c)
    val typeName = weakTypeOf[A].typeSymbol.name
    val rhs1 = Apply(Select(ev1.tree, c.universe.TermName("from" + typeName)), List(rhs.tree))
    c.Expr[R](Apply(Select(ev0, findMethodName(c)), List(lhs, rhs1)))
  }
  
  def operatorNames: Map[String, String]
}
  object Ops extends Ops {
    final val operatorNames = Map(
    // Eq (=== =!=)
    ("$eq$eq$eq", "eqv"),
    ("$eq$bang$eq", "neqv"),

    // Order (> >= < <=)
    ("$greater", "gt"),
    ("$greater$eq", "gteqv"),
    ("$less", "lt"),
    ("$less$eq", "lteqv"),

    // Semigroup (|+|)
    ("$bar$plus$bar", "op"),
    ("$bar$minus$bar", "opInverse"),

    // Ring (unary_- + - * **)
    ("unary_$minus", "negate"),
    ("$plus", "plus"),
    ("$minus", "minus"),
    ("$times", "times"),
    ("$times$times", "pow"),

    // EuclideanRing (/~ % /%)
    ("$div$tilde", "quot"),
    ("$percent", "mod"),
    ("$div$percent", "quotmod"),

    // Field (/)
    ("$div", "div"),

    // BooleanAlgebra (^ | & ~)
    ("$up", "xor"),
    ("$bar", "or"),
    ("$amp", "and"),
    ("unary_$tilde", "complement"),

    // BitString (<< >> >>>)
    ("$less$less", "leftShift"),
    ("$greater$greater$greater", "rightShift"),
    ("$greater$greater", "signedRightShift"),

    // VectorSpace (*: :* :/ ⋅)
    ("$times$colon", "timesl"),
    ("$colon$times", "timesr"),
    ("$colon$div", "divr"),
    ("$u22C5", "dot"),

    // GroupAction (|+|> <|+| +> <+ *> <*)
    ("$bar$plus$bar$greater", "gopl"),
    ("$less$bar$plus$bar", "gopr"),
    ("$plus$greater", "gplusl"),
    ("$less$plus", "gplusr"),
    ("$times$greater", "gtimesl"),
    ("$less$times", "gtimesr"),

    // Torsor (<|-|> <-> </>)
    ("$less$bar$minus$bar$greater", "pdiff"),
    ("$less$minus$greater", "pminus"),
    ("$less$div$greater", "pdiv")
  )
}

////////////////////////////////////////////////////////////////////////////////////////

// Implicit declarations
 object implicits extends AdditiveSemigroupSyntax 
 					 with IntInstances
                     with LongInstances
                     with FloatInstances
                     with DoubleInstances
                     with ArrayInstances
                      
trait AdditiveSemigroupSyntax {

  implicit def additiveSemigroupOps[A:AdditiveSemigroup](a:A) = new AdditiveSemigroupOps(a)
  implicit def literalIntAdditiveSemigroupOps(lhs:Int) = new LiteralIntAdditiveSemigroupOps(lhs)
  implicit def literalLongAdditiveSemigroupOps(lhs:Long) = new LiteralLongAdditiveSemigroupOps(lhs)
  implicit def literalDoubleAdditiveSemigroupOps(lhs:Double) = new LiteralDoubleAdditiveSemigroupOps(lhs)
}
                   
final class NoImplicit[A]

object NoImplicit {
  implicit def noImplicit0[A] = new NoImplicit[A]
  implicit def noImplicit1[A](implicit ev: A) = new NoImplicit[A]
}