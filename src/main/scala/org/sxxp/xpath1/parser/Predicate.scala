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

package org.sxxp.xpath1.parser

import org.sxxp.xpath1.parser.expression.Expression
import org.sxxp.xpath1.exp.XPathContext
import org.sxxp.xpath1.parser.types.{XBoolean, XNumber}
import org.sxxp.xpath1.parser.axis.NodeWithAncestors

case class Predicate(expr: Expression) {
  def evaluate(node: NodeWithAncestors, nodeIndex: Int, context: XPathContext): Boolean = {
    expr.evaluate(node, context) match {
      case XNumber(number) => nodeIndex == number.toInt
      case XBoolean(isTrue) => isTrue
      case _ => ???
    }
  }

}