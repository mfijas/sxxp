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

package org.sxxp.xpath1.parser.nodetest

import org.sxxp.xpath1.parser.QName
import scala.xml.Node

trait NodeTest {
  def apply(node: Node): Boolean = ???
}

case class NameNodeTest(name: QName) extends NodeTest {
  override def apply(node: Node): Boolean = {
    // TODO support namespaces
    name.localPart == "*" || node.label == name.localPart
  }
}

case class NodeTypeTest(nodeType: NodeType) extends NodeTest

sealed trait NodeType

case object CommentNodeType extends NodeType

case object TextNodeType extends NodeType

case object ProcessingInstructionNodeType extends NodeType

case object NodeNodeType extends NodeType

