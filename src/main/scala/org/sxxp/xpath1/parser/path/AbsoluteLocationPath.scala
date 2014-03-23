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

package org.sxxp.xpath1.parser.path

import org.sxxp.xpath1.parser.step.Step
import scala.xml.{TopScope, Node, Elem}
import org.sxxp.xpath1.exp.XPathContext
import org.sxxp.xpath1.utils.Logging
import org.sxxp.xpath1.parser.axis.NodeWithAncestors


/**
 * Location path starting with "/"
 */
case class AbsoluteLocationPath(steps: List[Step]) extends LocationPath with Logging {
  // TODO not so nice, think about doing it better
  private def dummyRoot(child: Node) =
    Elem(null, "dummy-root", scala.xml.Null, TopScope, false, child)

  override def select(currentNode: NodeWithAncestors, context: XPathContext) = {
    // HACK
    var curNodeSeq: Seq[NodeWithAncestors] = Seq(NodeWithAncestors(dummyRoot(context.rootNode.node), List.empty))
    // TODO verify this if clause
    for (step <- steps if !curNodeSeq.isEmpty) {
      logger.debug("select: step = {}", step)
      curNodeSeq = curNodeSeq.flatMap {
        node =>
          logger.debug("select: node = {}", node)
          step.select(node, context)
      }
    }
    curNodeSeq
  }
}

object AbsoluteLocationPath {
  def apply(path: LocationPath): AbsoluteLocationPath = AbsoluteLocationPath(path.steps)
}
