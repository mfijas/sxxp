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
    XBoolean(left.evaluate(node, context).asBoolean.isTrue || right.evaluate(node, context).asBoolean.isTrue)
}

case class AndExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(node: Node, context: XPathContext) =
    XBoolean(left.evaluate(node, context).asBoolean.isTrue && right.evaluate(node, context).asBoolean.isTrue)
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
    XNumber(left.evaluate(node, context).asNumber.number + right.evaluate(node, context).asNumber.number)
}

case class SubtractExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(node: Node, context: XPathContext) =
    XNumber(left.evaluate(node, context).asNumber.number - right.evaluate(node, context).asNumber.number)
}

case class MultiplyExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(node: Node, context: XPathContext) =
    XNumber(left.evaluate(node, context).asNumber.number * right.evaluate(node, context).asNumber.number)
}

case class DivExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(node: Node, context: XPathContext) =
    XNumber(left.evaluate(node, context).asNumber.number / right.evaluate(node, context).asNumber.number)
}

case class ModExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(node: Node, context: XPathContext) =
    XNumber(left.evaluate(node, context).asNumber.number % right.evaluate(node, context).asNumber.number)
}

case class MinusExpression(exp: Expression) extends Expression {
  override def evaluate(node: Node, context: XPathContext) =
    XNumber(-exp.evaluate(node, context).asNumber.number)
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
    XNodeSeq(expr.evaluate(node, context).asNodeSeq.nodeSeq.zipWithIndex.filter {
      case (node, index) =>
        predicate.evaluate(node, index + 1, context)
    }.map {
      case (node, _) =>
        node
    })
  }
}

case class PathExpression(expr: Expression, path: LocationPath) extends Expression

case class AbbreviatedPathExpression(expr: Expression, path: LocationPath) extends Expression

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