/**
 * Copyright (c) 2005, 2013, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */
package org.openjdk.jmh.samples

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import fastparse.all._

/*
trait ExprF[X]
// ... add your case class definitions for Lit and Add here ...
case class LitT[X](i: Int) extends ExprF[X]
case class AddT[X](lhs: X, rhs: X) extends ExprF[X]


// Similar to `IntList` we can recover the recursive datatype of expressions
// by using `Fix`:

type ExprT = Fix[ExprF]

case class ParseyT[V](alg: Expr[V]) {
  val number: P[V] = P(CharIn('0' to '9').rep(1).!.map(_.toInt)).map(alg.Lit(_))
  val parens: P[V] = P("(" ~/ addSub ~ ")")
  val factor: P[V] = P(number | parens)

  val divMul: P[V] = P(factor ~ (CharIn("*").! ~/ factor).rep).map(eval)
  val addSub: P[V] = P(divMul ~ (CharIn("+-").! ~/ divMul).rep).map(eval)
  val expr:   P[V] = P(addSub ~ End)

  def eval(tree: (V, Seq[(String, V)])) : V = {
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

 */

object ParseyOri {
  val number: P[Int] = P( CharIn('0'to'9').rep(1).!.map(_.toInt) )
  val parens: P[Int] = P( "(" ~/ addSub ~ ")" )
  val factor: P[Int] = P( number | parens )

  val divMul: P[Int] = P( factor ~ (CharIn("*/").! ~/ factor).rep ).map(eval)
  val addSub: P[Int] = P( divMul ~ (CharIn("+-").! ~/ divMul).rep ).map(eval)
  val expr: P[Int]   = P( addSub ~ End )
  def eval(tree: (Int, Seq[(String, Int)])) = {

    val (base, ops) = tree
    ops.foldLeft(base){ case (left, (op, right)) => op match{
                         case "+" => left + right case "-" => left - right
                         case "*" => left * right case "/" => left / right
                       }}
  }

}

case class Parsey[V](alg: Expr[V]) {
  val number: P[V] = P(CharIn('0' to '9').rep(1).!.map(_.toInt)).map(alg.Lit(_))
  val parens: P[V] = P("(" ~/ addSub ~ ")")
  val factor: P[V] = P(number | parens)

  val divMul: P[V] = P(factor ~ (CharIn("*").! ~/ factor).rep).map(eval)
  val addSub: P[V] = P(divMul ~ (CharIn("+-").! ~/ divMul).rep).map(eval)
  val expr:   P[V] = P(addSub ~ End)

  def eval(tree: (V, Seq[(String, V)])) : V = {
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

object IArr extends Expr[Array[Any]] {
  def Lit(i: Int) = {
    val arr = new Array[Any](2)
    arr(0) = i
    arr(1) = i.toString()
    arr
  }
  def Add(l: Array[Any], r: Array[Any]) = {
    val arr = new Array[Any](2)
    arr(0) = l(0).asInstanceOf[Int] + r(0).asInstanceOf[Int]
    arr(1) = "(" + l(1).asInstanceOf[String] + " + " + r(1).asInstanceOf[String] + ")"
    arr
  }
  def Sub(l: Array[Any], r: Array[Any]) = {
    val arr = new Array[Any](2)
    arr(0) = l(0).asInstanceOf[Int] - r(0).asInstanceOf[Int]
    arr(1) = "(" + l(1).asInstanceOf[String] + " + " + r(1).asInstanceOf[String] + ")"
    arr
  }

  def Mul(l: Array[Any], r: Array[Any]) = {
    val arr = new Array[Any](2)
    arr(0) = l(0).asInstanceOf[Int] * r(0).asInstanceOf[Int]
    arr(1) = "(" + l(1).asInstanceOf[String] + " + " + r(1).asInstanceOf[String] + ")"
    arr
  }
}

object IBlitz extends Expr[Array[Any]] {
  def Lit(x0: Int) = {
    val x2 = new Array[Any](2)
    val x3 = x2(0) = x0
    val x1 = (x0).toString()
    val x4 = x2(1) = x1
    x2
  }
  def Add(l: Array[Any], r: Array[Any]) = {
    val x27 = new Array[Any](2)
    val x7 = l
    val x9 = x7(0)
    val x10 = x9.asInstanceOf[Int]
    val x8 = r
    val x13 = x8(0)
    val x14 = x13.asInstanceOf[Int]
    val x17 = x10 + x14
    val x28 = x27(0) = x17
    val x15 = x8(1)
    val x16 = x15.asInstanceOf[java.lang.String]
    val x11 = x7(1)
    val x12 = x11.asInstanceOf[java.lang.String]
    val x26 = "(" + x12 + " + " + x16 + ")"
    val x29 = x27(1) = x26
    x27
  }
  def Sub(l: Array[Any], r: Array[Any]) = {
    val x27 = new Array[Any](2)
    val x7 = l
    val x9 = x7(0)
    val x10 = x9.asInstanceOf[Int]
    val x8 = r
    val x13 = x8(0)
    val x14 = x13.asInstanceOf[Int]
    val x17 = x10 - x14
    val x28 = x27(0) = x17
    val x15 = x8(1)
    val x16 = x15.asInstanceOf[java.lang.String]
    val x11 = x7(1)
    val x12 = x11.asInstanceOf[java.lang.String]
    val x26 = "(" + x12 + " - " + x16 + ")"
    val x29 = x27(1) = x26
    x27
  }

  def Mul(l: Array[Any], r: Array[Any]) = {
    val x27 = new Array[Any](2)
    val x7 = l
    val x9 = x7(0)
    val x10 = x9.asInstanceOf[Int]
    val x8 = r
    val x13 = x8(0)
    val x14 = x13.asInstanceOf[Int]
    val x17 = x10 * x14
    val x28 = x27(0) = x17
    val x15 = x8(1)
    val x16 = x15.asInstanceOf[java.lang.String]
    val x11 = x7(1)
    val x12 = x11.asInstanceOf[java.lang.String]
    val x26 = "(" + x12 + " * " + x16 + ")"
    val x29 = x27(1) = x26
    x27
  }
}


class JMHSample_01_HelloWorld {


  @Benchmark
  @Fork(jvmArgsAppend = Array("-XX:MaxInlineSize=0"))
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def TupleAlgTest(): Unit = {
    val algi = Combine(IPrint,IEval)
    val Parsed.Success(y,_) = Parsey(algi).expr.parse("1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2-1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2-1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2*(1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2-1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2-1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2)")
    //val Parsed.Success(z,_) = Parsey(algi).expr.parse("1+2+3*2-2*42-200-48+10")
    //val Parsed.Success(o,_) = Parsey(algi).expr.parse("10*1+3-100*2-2*42-200-48+10")

  }

  @Benchmark
  @Fork(jvmArgsAppend = Array("-XX:MaxInlineSize=0"))
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def StagedTest(): Unit = {
    val algi = IBlitz
    val Parsed.Success(y,_) = Parsey(algi).expr.parse("1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2-1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2-1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2*(1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2-1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2-1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2)")
    //val Parsed.Success(z,_) = Parsey(algi).expr.parse("1+2+3*2-2*42-200-48+10")
    //val Parsed.Success(o,_) = Parsey(algi).expr.parse("10*1+3-100*2-2*42-200-48+10")
  }

  @Benchmark
  @Fork(jvmArgsAppend = Array("-XX:MaxInlineSize=0"))
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def OptimalTest(): Unit = {
    val algi = IArr
    val Parsed.Success(y,_) = Parsey(algi).expr.parse("1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2-1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2-1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2*(1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2-1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2-1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2+1+2*3+4-23+1*3-2*3+2-3*2)")
    //val Parsed.Success(z,_) = Parsey(algi).expr.parse("1+2+3*2-2*42-200-48+10")
    //val Parsed.Success(o,_) = Parsey(algi).expr.parse("10*1+3-100*2-2*42-200-48+10")

  }


}
