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
import scala.xml.Node
import org.sxxp.xpath1.exp.XPathContext
import org.sxxp.xpath1.utils.Logging
import org.sxxp.xpath1.parser.axis.{DescendantOrSelfAxis, NodeWithAncestors}

/**
 * Location path starting with "//"
 */
case class AbbreviatedAbsoluteLocationPath(steps: List[Step]) extends LocationPath with Logging {
  override def :+(step: Step): LocationPath = AbbreviatedAbsoluteLocationPath(steps :+ step)

  override def select(currentNode: Node, context: XPathContext) = {
    // XXX this is obviously crap (should have currentNode as NodeWithAncestors
    var curNodeSeq: Seq[NodeWithAncestors] = DescendantOrSelfAxis(NodeWithAncestors(context.rootNode, List.empty))
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

object AbbreviatedAbsoluteLocationPath {
  def apply(path: LocationPath): AbbreviatedAbsoluteLocationPath = AbbreviatedAbsoluteLocationPath(path.steps)
}
