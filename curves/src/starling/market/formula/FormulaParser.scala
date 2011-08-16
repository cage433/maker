package starling.market.formula

import scala.util.parsing.combinator.lexical.StdLexical
import starling.quantity.{SimpleNamedQuantity, Quantity}

sealed abstract class Expr {
  def eval(): Quantity

  def children(): List[Expr]
}

case class EConst(value: Quantity) extends Expr {
  def eval(): Quantity = value

  def children() = Nil

  override def toString = value.toString
}

case class EAdd(left: Expr, right: Expr) extends Expr {
  def eval(): Quantity = left.eval + right.eval

  def children() = List(left, right)

  override def toString = left.toString + " + " + right.toString
}

case class ESub(left: Expr, right: Expr) extends Expr {
  def eval(): Quantity = left.eval - right.eval

  def children() = List(left, right)

  override def toString = left.toString + " - " + right.toString
}

case class EMul(left: Expr, right: Expr) extends Expr {
  def eval(): Quantity = left.eval * right.eval

  def children() = List(left, right)

  override def toString = left.toString + " * " + right.toString
}

case class EDiv(left: Expr, right: Expr) extends Expr {
  def eval(): Quantity = left.eval / right.eval

  def children() = List(left, right)

  override def toString = left.toString + " / " + right.toString
}

case class EUMinus(e: Expr) extends Expr {
  def eval(): Quantity = -e.eval

  def children() = List(e)

  override def toString = "-" + e
}

case class EParens(e: Expr) extends Expr {
  def eval(): Quantity = e.eval

  def children() = List(e)

  override def toString = "(" + e + ")"
}

case class EFunc(e: Expr, name: String, func: Expr => Expr) extends Expr {
  def eval() = {
    val result = func(e).eval()
    result
  }

  def children() = List(e)

  override def toString = name + "(" + e + ")"
}

class ExprLexical extends StdLexical {
  override def token: Parser[Token] = floatingToken | super.token

  def floatingToken: Parser[Token] =
    rep1(digit) ~ optFraction ~ optExponent ^^ {
      case intPart ~ frac ~ exp => NumericLit(
        (intPart mkString "") :: frac :: exp :: Nil mkString "")
    }

  def chr(c: Char) = elem("", ch => ch == c)

  def sign = chr('+') | chr('-')

  def optSign = opt(sign) ^^ {
    case None => ""
    case Some(sign) => sign
  }

  def fraction = '.' ~ rep(digit) ^^ {
    case dot ~ ff => dot :: (ff mkString "") :: Nil mkString ""
  }

  def optFraction = opt(fraction) ^^ {
    case None => ""
    case Some(fraction) => fraction
  }

  def exponent = (chr('e') | chr('E')) ~ optSign ~ rep1(digit) ^^ {
    case e ~ optSign ~ exp => e :: optSign :: (exp mkString "") :: Nil mkString ""
  }

  def optExponent = opt(exponent) ^^ {
    case None => ""
    case Some(exponent) => exponent
  }
}

import scala.util.parsing.combinator.syntactical._

class FormulaParser(userOps: Map[String, Quantity => Quantity] = Map(), named: Boolean = false) extends StandardTokenParsers {
  private val customOps: Map[String, Expr => Expr] = userOps.map{
    case (k, v) => (k, {e: Expr => EConst(v(e.eval()))})
  }

  private val unaryOps: Map[String, Expr => Expr] = customOps

  override val lexical = new ExprLexical
  lexical.delimiters ++= List("+", "-", "*", "/", "(", ")")
  lexical.reserved ++= unaryOps.keySet

  private implicit def map2Parser[V](m: Map[String, V]) = m.keys.map(_ ^^ (identity)).reduceLeft(_ | _)

  private def unaryFct: Parser[Expr] = unaryOps ~ "(" ~ expr ~ ")" ^^ {
    case op ~ _ ~ d ~ _ => EFunc(d, op, unaryOps(op))
  }

  def value = numericLit ^^ {
    s => if (named) {
      EConst(SimpleNamedQuantity("Const", s.toDouble))
    } else {
      EConst(s.toDouble)
    }
  }

  def parens: Parser[Expr] = "(" ~> expr <~ ")" ^^ {
    case d => EParens(d)
  }

  def unaryMinus: Parser[EUMinus] = "-" ~> term ^^ {
    EUMinus(_)
  }

  def term = (value | parens | unaryMinus | unaryFct)

  def binaryOp(level: Int): Parser[((Expr, Expr) => Expr)] = {
    level match {
      case 1 =>
        "+" ^^^ {
          (a: Expr, b: Expr) => EAdd(a, b)
        } |
          "-" ^^^ {
            (a: Expr, b: Expr) => ESub(a, b)
          }
      case 2 =>
        "*" ^^^ {
          (a: Expr, b: Expr) => EMul(a, b)
        } |
          "/" ^^^ {
            (a: Expr, b: Expr) => EDiv(a, b)
          }
      case _ => throw new RuntimeException("bad precedence level " + level)
    }
  }

  val minPrec = 1
  val maxPrec = 2

  private def binary(level: Int): Parser[Expr] =
    if (level > maxPrec) term
    else binary(level + 1) * binaryOp(level)

  def expr: Parser[Expr] = (binary(minPrec) | term)

  private def parse(s: String) = {
    val tokens = new lexical.Scanner(s.replace(" ", "").toLowerCase)
    phrase(expr)(tokens)
  }

  def eval(s: String): Quantity = parse(s) match {
    case Success(tree, _) => {
      tree.eval
    }
    case e: NoSuccess =>
      throw new IllegalArgumentException("Bad syntax: " + s)
  }


  def apply(s: String): Expr = {
    parse(s) match {
      case Success(tree, _) => tree
      case e: NoSuccess =>
        throw new IllegalArgumentException("Bad syntax: " + s)
    }
  }

  def test(exprstr: String) = {
    parse(exprstr) match {
      case Success(tree, _) =>
        println("Tree: " + tree)
        val v = tree.eval()
        println("Eval: " + v)
        tree
      case e: NoSuccess => Console.err.println(e)
    }
  }

}
