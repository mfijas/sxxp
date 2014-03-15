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

import org.scalatest.FlatSpec

class AxisTest extends FlatSpec {

  def extractNodes(axis: Seq[NodeWithAncestors]) = axis.map(_.node)

  "ChildAxis" should "select child nodes" in {
    // given
    val xml =
      <root>
        <a></a>
        <b></b>
      </root>

    // when
    val axis = ChildAxis(NodeWithAncestors(xml, List.empty))

    assert(extractNodes(axis) === xml.child)
    axis.foreach {
      nwa => assert(nwa.ancestors === List(xml))
    }
  }

  "DescendantAxis" should "select all descendant nodes" in {
    // given
    val root =
      <root>
        <a>
          <b>1</b>
          <b>2</b>
        </a>
        <a>
          <c>3</c>
        </a>
      </root>

    // when
    val axis = DescendantAxis(NodeWithAncestors(root, List.empty)).toList

    // then
    assert(extractNodes(axis) === root.descendant)

    val descendant0 = axis(1) // strange indices because of whitespace nodes
    val a = (root \ "a")(0)
    assert(descendant0.node === a)
    assert(descendant0.ancestors === List(root))

    val descendant1 = axis(3) // strange indices because of whitespace nodes
    val b = (root \ "a" \ "b")(0)
    assert(descendant1.node === b)
    assert(descendant1.ancestors === List(a, root))
  }

  "DescendantOrSelfAxis" should "select node and all descendant nodes" in {
    // given
    val xml =
      <root>
        <a>
          <b>1</b>
          <b>2</b>
        </a>
        <a>
          <c>3</c>
        </a>
      </root>

    // when
    val axis = DescendantOrSelfAxis(NodeWithAncestors(xml, List.empty))

    // then
    assert(extractNodes(axis) === xml.descendant_or_self)
  }

  "ParentAxis" should "select parent node" in {
    // given
    val root =
      <root>
        <a>
          <b/>
        </a>
      </root>
    val a = (root \ "a").head
    val b = (root \\ "b").head

    // when
    val axis = ParentAxis(NodeWithAncestors(b, List(a, root)))
    val node = axis.head.node
    val ancestors = axis.head.ancestors

    // then
    assert(node === a)
    assert(ancestors === List(root))
  }

  "AncestorAxis" should "select ancestors in order" in {
    // given
    val root =
      <root>
        <a>
          <b/>
        </a>
      </root>
    val a = (root \ "a").head
    val b = (root \\ "b").head

    // when
    val axis = AncestorAxis(NodeWithAncestors(b, List(a, root)))

    // then
    assert(extractNodes(axis) === List(a, root))
  }

  "AncestorOrSelfAxis" should "select self and ancestors in order" in {
    // given
    val root =
      <root>
        <a>
          <b/>
        </a>
      </root>
    val a = (root \ "a").head
    val b = (root \\ "b").head

    // when
    val axis = AncestorOrSelfAxis(NodeWithAncestors(b, List(a, root)))

    // then
    assert(extractNodes(axis) === List(b, a, root))
  }

  "SelfAxis" should "select self" in {
    // given
    val elem = <elem/>

    // when
    val axis = SelfAxis(NodeWithAncestors(elem, List.empty))

    // then
    assert(axis.head.node === elem)
  }

  "FollowingSiblingAxis" should "select all following siblings" in {
    // given
    val root =
      <root>
        <a/>
        <b/>
        <c/>
        <d/>
      </root>
    val b = (root \ "b")(0)

    // when
    val axis = FollowingSiblingAxis(NodeWithAncestors(b, List(root)))

    // then
    assert(extractNodes(axis) === root.child.drop(4))
  }

  "FollowingAxis" should "select all nodes following in the document" in {
    // given
    val root =
      <root><a><aa/></a><b><bb1><bbb/></bb1><bb2><bbb/></bb2></b><c><cc/></c></root>

    val b = (root \ "b").head
    val bb1 = (b \ "bb1").head
    val bb2 = (root \\ "bb2").head
    val bbb = (bb2 \ "bbb").head
    val c = (root \\ "c").head
    val cc = (c \ "cc").head

    // when
    val axis = FollowingAxis(NodeWithAncestors(bb1, List(b, root)))

    // then
    assert(extractNodes(axis).toList === List(bb2, bbb, c, cc))
  }

  "PrecedingSiblingAxis" should "select all preceding siblings" in {
    // given
    val root =
      <root>
        <a/>
        <b/>
        <c/>
        <d/>
      </root>
    val c = (root \ "c")(0)

    // when
    val axis = PrecedingSiblingAxis(NodeWithAncestors(c, List(root)))

    // then
    assert(extractNodes(axis) === root.child.take(5).reverse) // including whitespace nodes
  }

  "PrecedingAxis" should "select all nodes preceding in the document" in {
    // given
    val root =
      <root><a><aa/><ab/></a><b><bb1><bbb/></bb1><bb2><bbb/></bb2></b><c><cc/></c></root>

    val a = (root \ "a").head
    val aa = (a \ "aa").head
    val ab = (a \ "ab").head
    val b = (root \ "b").head
    val bb1 = (b \ "bb1").head
    val bb2 = (b \\ "bb2").head
    val bbb = (bb2 \ "bbb").head

    // when
    val axis = PrecedingAxis(NodeWithAncestors(bb2, List(b, root)))

    // then
    assert(extractNodes(axis).toList === List(a, aa, ab, bb1, bbb))
  }
}
