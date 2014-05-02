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
    nodeWithAncestors.parent.fold(Stream.empty[NodeWithAncestors])(p => p #:: AncestorAxis(p))

  override def toString = "AncestorAxis"
}

object AncestorOrSelfAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors) =
    nodeWithAncestors #:: AncestorAxis(nodeWithAncestors)

  override def toString = "AncestorOrSelfAxis"
}

object AttributeAxis extends Axis {
  // attributes are not nodes in Scala XML :(
  override def apply(nodeWithAncestors: NodeWithAncestors) = ???

  override def toString = "AttributeAxis"
}

object ChildAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors) = {
    val node = nodeWithAncestors.node
    val ancestorsForChild = node :: nodeWithAncestors.ancestors
    node.child.toStream.zipWithIndex.map {
      case (child, index) => NodeWithAncestors(child, ancestorsForChild, nodeWithAncestors.path.append(index))
    }
  }

  override def toString = "ChildAxis"
}

object DescendantAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors): Stream[NodeWithAncestors] = {
    val node = nodeWithAncestors.node
    val ancestorsForChild = node :: nodeWithAncestors.ancestors
    node.child.toStream.zipWithIndex.flatMap {
      case (child, index) =>
        val childWithParent = NodeWithAncestors(child, ancestorsForChild, nodeWithAncestors.path.append(index))
        childWithParent #:: DescendantAxis(childWithParent)
    }
  }

  override def toString = "DescendantAxis"
}

object DescendantOrSelfAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors) =
    nodeWithAncestors #:: DescendantAxis(nodeWithAncestors)

  override def toString = "DescendantOrSelfAxis"
}

object FollowingAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors) =
    AncestorOrSelfAxis(nodeWithAncestors).flatMap(n => FollowingSiblingAxis(n).flatMap(n => DescendantOrSelfAxis(n)))

  override def toString = "FollowingAxis"
}

object FollowingSiblingAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors) = {
    val NodeWithAncestors(_, parents, nodePath) = nodeWithAncestors
    if (!parents.isEmpty) {
      val parentNode = parents.head
      val parentPath = nodePath.parentPath
      val nodeIndex = nodePath.path.last
      parentNode.child.toStream.zipWithIndex.drop(nodeIndex + 1).map {
        case (n, index) => NodeWithAncestors(n, parents, parentPath.append(index))
      }
    } else Stream.empty
  }

  override def toString = "FollowingSiblingAxis"
}

// not as easy and not that important at the moment
object NamespaceAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors) = ???

  override def toString = "NamespaceAxis"
}

object ParentAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors) =
    nodeWithAncestors.parent.toSeq

  override def toString = "ParentAxis"
}

object PrecedingAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors) = {
    // reverse is inefficient because it realizes whole stream -- can we do better?
    AncestorOrSelfAxis(nodeWithAncestors).reverse.flatMap {
      n => PrecedingSiblingAxis(n).flatMap {
        n => DescendantOrSelfAxis(n)
      }
    }.reverse
  }

  override def toString = "PrecedingAxis"
}

object PrecedingSiblingAxis extends Axis {
  override def apply(nodeWithAncestors: NodeWithAncestors) = {
    val NodeWithAncestors(_, parents, nodePath) = nodeWithAncestors
    if (!parents.isEmpty) {
      val parentNode = parents.head
      val parentPath = nodePath.parentPath
      val nodeIndex = nodePath.path.last
      parentNode.child.toStream.zipWithIndex.take(nodeIndex).reverse.map {
        case (n, index) => NodeWithAncestors(n, parents, parentPath.append(index))
      }
    } else Stream.empty
  }

  override def toString = "PrecedingSiblingAxis"
}

object SelfAxis extends Axis {
  override def apply(node: NodeWithAncestors) = Seq(node)

  override def toString = "SelfAxis"
}
