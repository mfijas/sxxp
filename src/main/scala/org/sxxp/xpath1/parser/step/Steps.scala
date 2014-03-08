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

package org.sxxp.xpath1.parser.step

import scala.xml.{NodeSeq, Node}
import org.sxxp.xpath1.parser.Predicate
import org.sxxp.xpath1.parser.nodetest.NodeTest
import org.sxxp.xpath1.exp.XPathContext
import org.sxxp.xpath1.utils.Logging
import org.sxxp.xpath1.parser.axis.{NodeWithAncestors, Axis}

trait Step {
  def select(node: Node, context: XPathContext): NodeSeq = ???
}

/**
 * "."
 */
case object CurNodeStep extends Step {
  override def select(node: Node, context: XPathContext): NodeSeq = node
}

/**
 * ".."
 */
case object ParentNodeStep extends Step

case class NodeStep(axis: Axis, nodeTest: NodeTest, predicates: List[Predicate]) extends Step with Logging {
  override def select(node: Node, context: XPathContext): NodeSeq = {
    // TODO migrate fully to NodeWithAncestors
    val seq = axis(NodeWithAncestors(node, List.empty)).filter(n => nodeTest(n.node)).zipWithIndex.filter {
      case (n, index) =>
        predicates.forall {
          predicate =>
            val result = predicate.evaluate(n.node, index + 1, context)
            logger.debug("predicate: {}, result: {}", predicate, result)
            result
        }
    }.map(_._1.node)
    NodeSeq.fromSeq(seq)
  }
}


/**
 * //
 */
// TODO consider changing to DescendantAxis
case class AbbreviatedNodeStep(step: Step) extends Step {
  override def select(node: Node, context: XPathContext): NodeSeq = {
    NodeSeq.fromSeq(node.descendant_or_self.flatMap(node => step.select(node, context)))
  }
}

