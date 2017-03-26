import fastparse.all._
case class Parsey[V](alg: Expr[V]) {
  val number: P[V] =
    P(CharIn('0' to '9').rep(1).!.map(_.toInt)).map(alg.Lit(_))
  val parens: P[V] = P("(" ~/ addSub ~ ")")
  val factor: P[V] = P(number | parens)

  val divMul: P[V] = P(factor ~ (CharIn("*").! ~/ factor).rep).map(eval)
  val addSub: P[V] = P(divMul ~ (CharIn("+-").! ~/ divMul).rep).map(eval)
  val expr: P[V] = P(addSub ~ End)

  def eval(tree: (V, Seq[(String, V)])): V = {
    val (base, ops) = tree
    ops.foldLeft(base) {
      case (left, (op, right)) =>
        op match {
          case "+" => alg.Add(left, right)
          case "-" => alg.Sub(left, right)
          case "*" => alg.Mul(left, right)
        }
    }
  }
}

// Example:
// println(Parsey(IEval).expr.parse("1+2*3"))
// println(Parsey(IPrint).expr.parse("1+2*3"))
// println(Parsey(Combine(IPrint,IEval)).expr.parse("1+2*3"))

trait Expr[V] {
  def Lit(i: Int): V
  def Add(l: V, r: V): V
  def Sub(l: V, r: V): V
  def Mul(l: V, r: V): V
}

object IPrint extends Expr[String] {
  def Add(l: String, r: String) = "(" ++ l ++ " + " ++ r ++ ")"
  def Sub(l: String, r: String) = "(" ++ l ++ " - " ++ r ++ ")"
  def Mul(l: String, r: String) = "(" ++ l ++ " * " ++ r ++ ")"
  def Lit(i: Int) = i.toString
}

object IEval extends Expr[Int] {
  def Lit(i: Int) = i
  def Add(l: Int, r: Int) = l + r
  def Sub(l: Int, r: Int) = l - r
  def Mul(l: Int, r: Int) = l * r
}

case class Combine[V, W](alg1: Expr[V], alg2: Expr[W]) extends Expr[(V, W)] {
  def Lit(i: Int) = (alg1.Lit(i), alg2.Lit(i))
  def Add(l: (V, W), r: (V, W)) = (alg1.Add(l._1, r._1), alg2.Add(l._2, r._2))
  def Sub(l: (V, W), r: (V, W)) = (alg1.Sub(l._1, r._1), alg2.Sub(l._2, r._2))
  def Mul(l: (V, W), r: (V, W)) = (alg1.Mul(l._1, r._1), alg2.Mul(l._2, r._2))
}
