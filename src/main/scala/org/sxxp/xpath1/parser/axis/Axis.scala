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


abstract class Axis {
  def apply(nodeWithAncestors: NodeWithAncestors): Seq[NodeWithAncestors]
}

object AncestorAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors): Stream[NodeWithAncestors] =
    nodeWithAncestors.parent.map(p => p #:: AncestorAxis(p)).getOrElse(Stream.empty)
}

object AncestorOrSelfAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors) =
    nodeWithAncestors #:: AncestorAxis(nodeWithAncestors)
}

object AttributeAxis extends Axis {
  // attributes are not nodes in Scala XML :(
  override def apply(nodeWithAncestors: NodeWithAncestors) = ???
}

object ChildAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors) = {
    val node = nodeWithAncestors.node
    val ancestorsForChild = node :: nodeWithAncestors.ancestors
    node.child.toStream.map(NodeWithAncestors(_, ancestorsForChild))
  }
}

object DescendantAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors): Stream[NodeWithAncestors] = {
    val node = nodeWithAncestors.node
    val ancestorsForChild = node :: nodeWithAncestors.ancestors
    node.child.toStream.flatMap {
      child =>
        val childWithParent = NodeWithAncestors(child, ancestorsForChild)
        childWithParent #:: DescendantAxis(childWithParent)
    }
  }
}

object DescendantOrSelfAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors) =
    nodeWithAncestors #:: DescendantAxis(nodeWithAncestors)
}

object FollowingAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors) =
    AncestorOrSelfAxis(nodeWithAncestors).flatMap(n => FollowingSiblingAxis(n).flatMap(n => DescendantOrSelfAxis(n)))
}

object FollowingSiblingAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors) = {
    val NodeWithAncestors(node, parents) = nodeWithAncestors
    if (!parents.isEmpty) {
      val parent = parents.head
      val nodeIndex = parent.child.indexOf(node)
      parent.child.drop(nodeIndex + 1).toStream.map(n => NodeWithAncestors(n, parents))
    } else Stream.empty
  }
}

// not as easy and not that important at the moment
object NamespaceAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors) = ???
}

object ParentAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors) =
    nodeWithAncestors.parent.toSeq
}

object PrecedingAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors) = {
    // reverse is inefficient because it realizes whole stream -- can we do better?
    AncestorOrSelfAxis(nodeWithAncestors).reverse.flatMap(n => PrecedingSiblingAxis(n).flatMap(n => DescendantOrSelfAxis(n)))
  }
}

object PrecedingSiblingAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors) = {
    val NodeWithAncestors(node, parents) = nodeWithAncestors
    if (!parents.isEmpty) {
      val parent = parents.head
      val nodeIndex = parent.child.indexOf(node)
      parent.child.take(nodeIndex).reverse.toStream.map(n => NodeWithAncestors(n, parents))
    } else Stream.empty
  }
}

object SelfAxis extends Axis {
  override def apply(node: NodeWithAncestors) = Seq(node)
}