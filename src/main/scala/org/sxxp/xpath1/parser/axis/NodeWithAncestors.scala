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

package org.sxxp.xpath1.parser.axis

import scala.xml.Node

case class NodeWithAncestors(node: Node, ancestors: List[Node], path: NodePath) extends Ordered[NodeWithAncestors] {
  def parent = ancestors match {
    case head :: tail => Some(NodeWithAncestors(head, tail, path.parentPath))
    case List() => None
  }

  override def compare(that: NodeWithAncestors) = path.compare(that.path)
}

object NodeWithAncestors {
  def rootNode(node: Node) = new NodeWithAncestors(node, List.empty, NodePath.empty)
}
