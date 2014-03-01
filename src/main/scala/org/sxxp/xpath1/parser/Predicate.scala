package org.sxxp.xpath1.parser

import org.sxxp.xpath1.parser.expression.Expression
import scala.xml.Node
import org.sxxp.xpath1.exp.XPathContext
import org.sxxp.xpath1.parser.types.{XBoolean, XNumber}

case class Predicate(expr: Expression) {
  def evaluate(node: Node, nodeIndex: Int, context: XPathContext): Boolean = {
    expr.evaluate(node, context) match {
      case XNumber(number) => nodeIndex == number.toInt
      case XBoolean(isTrue) => isTrue
      case __ => ???
    }
  }

}