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
    val axis = ChildAxis(NodeWithAncestors.rootNode(xml))

    assert(extractNodes(axis) === xml.child)
    axis.zipWithIndex.foreach {
      case (nwa, index) =>
        assert(nwa.ancestors === List(xml))
        assert(nwa.path === NodePath(index))
    }
  }

  it should "return its name from toString" in {
    assert(ChildAxis.toString === "ChildAxis")
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
    val axis = DescendantAxis(NodeWithAncestors.rootNode(root))

    // then
    assert(extractNodes(axis) === root.descendant)

    val descendant0 = axis(1) // strange indices because of whitespace nodes
    val a = (root \ "a")(0)
    assert(descendant0.node === a)
    assert(descendant0.ancestors === List(root))
    assert(descendant0.path === NodePath(1))

    val descendant1 = axis(3) // strange indices because of whitespace nodes
    val b = (root \ "a" \ "b")(0)
    assert(descendant1.node === b)
    assert(descendant1.ancestors === List(a, root))
    assert(descendant1.path === NodePath(1, 1))

    val descendant2 = axis(6) // strange indices because of whitespace nodes
    val b2 = (root \ "a" \ "b")(1)
    assert(descendant2.node === b2)
    assert(descendant2.ancestors === List(a, root))
    assert(descendant2.path === NodePath(1, 3))
  }

  it should "return its name from toString" in {
    assert(DescendantAxis.toString === "DescendantAxis")
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
    val axis = DescendantOrSelfAxis(NodeWithAncestors.rootNode(xml))

    // then
    assert(extractNodes(axis) === xml.descendant_or_self)
  }

  it should "return its name from toString" in {
    assert(DescendantOrSelfAxis.toString === "DescendantOrSelfAxis")
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
    val axis = ParentAxis(NodeWithAncestors(b, List(a, root), NodePath(0, 0)))
    val parent = axis.head
    val node = parent.node
    val ancestors = parent.ancestors
    val path = parent.path

    // then
    assert(node === a)
    assert(ancestors === List(root))
    assert(path === NodePath(0))
  }

  it should "return its name from toString" in {
    assert(ParentAxis.toString === "ParentAxis")
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
    val axis = AncestorAxis(NodeWithAncestors(b, List(a, root), NodePath(0, 0)))

    // then
    assert(extractNodes(axis) === List(a, root))
    assert(axis(0).path === NodePath(0))
    assert(axis(1).path === NodePath())
  }

  it should "return its name from toString" in {
    assert(AncestorAxis.toString === "AncestorAxis")
  }

  "AncestorOrSelfAxis" should "select self and ancestors in order" in {
    // given
    val root = <root><a><b/></a></root>
    val a = (root \ "a").head
    val b = (root \\ "b").head

    // when
    val axis = AncestorOrSelfAxis(NodeWithAncestors(b, List(a, root), NodePath(0, 0)))

    // then
    assert(extractNodes(axis) === List(b, a, root))
    assert(axis(0).path === NodePath(0, 0))
    assert(axis(1).path === NodePath(0))
    assert(axis(2).path === NodePath())
  }

  it should "return its name from toString" in {
    assert(AncestorOrSelfAxis.toString === "AncestorOrSelfAxis")
  }

  "SelfAxis" should "select self" in {
    // given
    val elem = <elem/>

    // when
    val axis = SelfAxis(NodeWithAncestors.rootNode(elem))

    // then
    assert(axis.head.node === elem)
  }

  it should "return its name from toString" in {
    assert(SelfAxis.toString === "SelfAxis")
  }

  "FollowingSiblingAxis" should "select all following siblings" in {
    // given
    val root = <root><a/><b/><c/><d/></root>
    val b = (root \ "b")(0)

    // when
    val axis = FollowingSiblingAxis(NodeWithAncestors(b, List(root), NodePath(1)))

    // then
    assert(extractNodes(axis) === root.child.drop(2))
    assert(axis(0).path === NodePath(2))
    assert(axis(1).path === NodePath(3))
  }

  it should "return its name from toString" in {
    assert(FollowingSiblingAxis.toString === "FollowingSiblingAxis")
  }

  "FollowingAxis" should "select all nodes following in the document" in {
    // given
    val root =  <root><a><aa/></a><b><bb1><bbb/></bb1><bb2><bbb/></bb2></b><c><cc/></c></root>

    val b = (root \ "b").head
    val bb1 = (b \ "bb1").head
    val bb2 = (root \\ "bb2").head
    val bbb = (bb2 \ "bbb").head
    val c = (root \\ "c").head
    val cc = (c \ "cc").head

    // when
    val axis = FollowingAxis(NodeWithAncestors(bb1, List(b, root), NodePath(1, 0)))

    // then
    assert(extractNodes(axis).toList === List(bb2, bbb, c, cc))
    assert(axis(0).path === NodePath(1, 1))
    assert(axis(1).path === NodePath(1, 1, 0))
    assert(axis(2).path === NodePath(2))
    assert(axis(3).path === NodePath(2, 0))
  }

  it should "return its name from toString" in {
    assert(FollowingAxis.toString === "FollowingAxis")
  }

  "PrecedingSiblingAxis" should "select all preceding siblings" in {
    // given
    val root = <root><a/><b/><c/><d/></root>
    val c = (root \ "c")(0)

    // when
    val axis = PrecedingSiblingAxis(NodeWithAncestors(c, List(root), NodePath(2)))

    // then
    assert(extractNodes(axis) === root.child.take(2).reverse)
    assert(axis(0).path === NodePath(1))
    assert(axis(1).path === NodePath(0))
  }

  it should "return its name from toString" in {
    assert(PrecedingSiblingAxis.toString === "PrecedingSiblingAxis")
  }

  "PrecedingAxis" should "select all nodes preceding in the document" in {
    // given
    // xml in one line to avoid whitespace text nodes
    val root = <root><a><aa/><ab/></a><b><bb1><bbb/></bb1><bb2><bbb/></bb2></b><c><cc/></c></root>
    //      <root>
    //        <a>         0
    //         <aa/>        0
    //         <ab/>        1
    //        </a>
    //        <b>         1
    //          <bb1>       0
    //            <bbb/>      0
    //          </bb1>
    //          <bb2>       1
    //            <bbb/>      0
    //          </bb2>
    //        </b>
    //        <c>         2
    //          <cc/>       0
    //        </c>
    //      </root>

    val a = (root \ "a").head
    val aa = (a \ "aa").head
    val ab = (a \ "ab").head
    val b = (root \ "b").head
    val bb1 = (b \ "bb1").head
    val bb2 = (b \\ "bb2").head
    val bbb = (bb2 \ "bbb").head

    // when
    val axis = PrecedingAxis(NodeWithAncestors(bb2, List(b, root), NodePath(1, 1)))

    // then
    assert(extractNodes(axis).toList === List(bbb, bb1, ab, aa, a))
    assert(axis(0).path === NodePath(1, 0, 0))
    assert(axis(1).path === NodePath(1, 0))
    assert(axis(2).path === NodePath(0, 1))
    assert(axis(3).path === NodePath(0, 0))
    assert(axis(4).path === NodePath(0))
  }

  it should "return its name from toString" in {
    assert(PrecedingAxis.toString === "PrecedingAxis")
  }

  "AttributeAxis" should "return its name from toString" in {
    assert(AttributeAxis.toString === "AttributeAxis")
  }

  "NamespaceAxis" should "return its name from toString" in {
    assert(NamespaceAxis.toString === "NamespaceAxis")
  }

}
