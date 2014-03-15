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

import org.sxxp.xpath1.parser.Predicate
import org.sxxp.xpath1.parser.nodetest.NodeTest
import org.sxxp.xpath1.exp.XPathContext
import org.sxxp.xpath1.utils.Logging
import org.sxxp.xpath1.parser.axis.{DescendantOrSelfAxis, NodeWithAncestors, Axis}

trait Step {
  def select(node: NodeWithAncestors, context: XPathContext): Seq[NodeWithAncestors]
}

/**
 * "."
 */
case object CurNodeStep extends Step {
  override def select(node: NodeWithAncestors, context: XPathContext) =
    Seq(node)
}

/**
 * ".."
 */
case object ParentNodeStep extends Step {
  override def select(node: NodeWithAncestors, context: XPathContext) =
    Seq(NodeWithAncestors(node.ancestors.head, node.ancestors.tail))
}

case class NodeStep(axis: Axis, nodeTest: NodeTest, predicates: List[Predicate]) extends Step with Logging {
  override def select(node: NodeWithAncestors, context: XPathContext) = {
    axis(node).filter(n => nodeTest(n.node)).zipWithIndex.filter {
      case (n, index) =>
        predicates.forall {
          predicate =>
            val result = predicate.evaluate(n.node, index + 1, context)
            logger.debug("predicate: {}, result: {}", predicate, result)
            result
        }
    }.map(_._1)
  }
}


/**
 * //
 */
case class AbbreviatedNodeStep(step: Step) extends Step {
  override def select(node: NodeWithAncestors, context: XPathContext) =
    DescendantOrSelfAxis(node).flatMap(n => step.select(n, context))
}

