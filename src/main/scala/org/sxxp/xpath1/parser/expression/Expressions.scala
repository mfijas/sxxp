/*
 * Copyright 2014 Michał Fijas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.sxxp.xpath1.parser.expression

import scala.xml.Node
import org.sxxp.xpath1.parser.path.LocationPath
import org.sxxp.xpath1.parser.types._
import org.sxxp.xpath1.parser.QName
import org.sxxp.xpath1.parser.function._
import org.sxxp.xpath1.parser.types.XBoolean
import org.sxxp.xpath1.parser.types.XNumber
import org.sxxp.xpath1.parser.types.XString
import org.sxxp.xpath1.parser.types.XNodeSeq
import org.sxxp.xpath1.parser.Predicate
import org.sxxp.xpath1.exp.XPathContext

trait Expression {
  def evaluate(node: Node, context: XPathContext): XObject = ???
}

case class OrExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(node: Node, context: XPathContext) =
    XBoolean(left.evaluate(node, context).asBoolean.value || right.evaluate(node, context).asBoolean.value)
}

case class AndExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(node: Node, context: XPathContext) =
    XBoolean(left.evaluate(node, context).asBoolean.value && right.evaluate(node, context).asBoolean.value)
}

case class EqExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(node: Node, context: XPathContext) =
    XBoolean(XObjectComparator.isEqual(left.evaluate(node, context), right.evaluate(node, context)))
}

case class NeqExpression(left: Expression, right: Expression) extends Expression

case class LtExpression(left: Expression, right: Expression) extends Expression

case class GtExpression(left: Expression, right: Expression) extends Expression

case class LtEExpression(left: Expression, right: Expression) extends Expression

case class GtEExpression(left: Expression, right: Expression) extends Expression

case class SumExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(node: Node, context: XPathContext) =
    XNumber(left.evaluate(node, context).asNumber.value + right.evaluate(node, context).asNumber.value)
}

case class SubtractExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(node: Node, context: XPathContext) =
    XNumber(left.evaluate(node, context).asNumber.value - right.evaluate(node, context).asNumber.value)
}

case class MultiplyExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(node: Node, context: XPathContext) =
    XNumber(left.evaluate(node, context).asNumber.value * right.evaluate(node, context).asNumber.value)
}

case class DivExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(node: Node, context: XPathContext) =
    XNumber(left.evaluate(node, context).asNumber.value / right.evaluate(node, context).asNumber.value)
}

case class ModExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(node: Node, context: XPathContext) =
    XNumber(left.evaluate(node, context).asNumber.value % right.evaluate(node, context).asNumber.value)
}

case class MinusExpression(exp: Expression) extends Expression {
  override def evaluate(node: Node, context: XPathContext) =
    XNumber(-exp.evaluate(node, context).asNumber.value)
}

case class UnionExpression(left: Expression, right: Expression) extends Expression

case class LiteralExpression(value: String) extends Expression {
  override def evaluate(node: Node, context: XPathContext): XObject = XString(value)
}

case class NumberExpression(value: Double) extends Expression {
  override def evaluate(node: Node, context: XPathContext): XObject = XNumber(value)
}

case class FilterExpression(expr: Expression, predicate: Predicate) extends Expression {
  override def evaluate(node: Node, context: XPathContext): XObject = {
    XNodeSeq(expr.evaluate(node, context).asNodeSeq.value.zipWithIndex.filter {
      case (n, index) =>
        predicate.evaluate(n, index + 1, context)
    }.map {
      case (n, _) =>
        n
    })
  }
}

case class PathExpression(expr: Expression, path: LocationPath) extends Expression {
  override def evaluate(node: Node, context: XPathContext) = {
    val nodeSeq = expr.evaluate(node, context).asNodeSeq.value
    XNodeSeq(nodeSeq.flatMap(n => path.select(n, context)))
  }
}

case class AbbreviatedPathExpression(expr: Expression, path: LocationPath) extends Expression {
  override def evaluate(node: Node, context: XPathContext) = {
    val nodeSeq = expr.evaluate(node, context).asNodeSeq.value
    // TODO optimize it
    val descendants = nodeSeq.flatMap(n => n.descendant_or_self)
    XNodeSeq(descendants.flatMap(n => path.select(n, context)))
  }
}

case class FunctionCallExpression(functionName: QName, arguments: List[Expression]) extends Expression {

  override def evaluate(node: Node, context: XPathContext) = {

    def assertArity(arity: Int) =
      if (arity != arguments.length)
        throw new IllegalArgumentException(s"Function $functionName expects $arity argument(s), found ${arguments.length}!")

    def invokeFunction0(f: ParameterlessFunction) = {
      assertArity(0)
      f.apply()
    }

    def invokeFunction1(f: UnaryFunction) = {
      assertArity(1)
      f(arguments.head.evaluate(node, context))
    }

    functionName match {
      case NumberFunction.QNAME => invokeFunction1(NumberFunction)
      case BooleanFunction.QNAME => invokeFunction1(BooleanFunction)
      case StringFunction.QNAME => invokeFunction1(StringFunction)
      case TrueFunction.QNAME => invokeFunction0(TrueFunction)
      case FalseFunction.QNAME => invokeFunction0(FalseFunction)
      case _ => throw new IllegalStateException(s"Unknown function ${functionName.getFullName}!")
    }
  }
}