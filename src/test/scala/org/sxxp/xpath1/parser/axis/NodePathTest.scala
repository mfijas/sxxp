package org.sxxp.xpath1.parser.axis

import org.scalatest.FlatSpec

class NodePathTest extends FlatSpec {

  "NodePath" should "allow creation of root path" in {

    // when
    val nodePath = NodePath.root

    // then
    assert(nodePath.path === List())
  }

  it should "allow appending path segment to root path" in {

    // given
    val emptyPath = NodePath.root
    val childIndex = 3

    // when
    val childPath = emptyPath.append(childIndex)

    // then
    assert(childPath.path === List(childIndex))
  }

  it should "allow appending path segment to non-empty path" in {
    // given
    val path = new NodePath(Vector(1, 2))
    val childIndex = 3

    // when
    val childPath = path.append(childIndex)

    // then
    assert(childPath.path === List(1, 2, 3))
  }

  it should "allow obtaining parent path" in {
    // given
    val path = new NodePath(Vector(1, 2, 3))

    // when
    val parentPath = path.parentPath

    // then
    assert(parentPath.path === List(1, 2))
  }

  "NodePath.compareTo" should "compare paths" in {
    // given
    val p1 = new NodePath(Vector(1))
    val p12 = new NodePath(Vector(1, 2))
    val p12b = new NodePath(Vector(1, 2))
    val p13 = new NodePath(Vector(1, 3))

    // then
    assert(p12.compare(p12b) == 0)
    assert(p1.compare(p12) == -1)
    assert(p12.compare(p1) == 1)
    assert(p12.compare(p13) == -1)
  }

  "NodePath" should "allow using <, >, != and == operators" in {
    // given
    val p1 = new NodePath(Vector(1))
    val p12 = new NodePath(Vector(1, 2))
    val p12b = new NodePath(Vector(1, 2))
    val p13 = new NodePath(Vector(1, 3))

    // then
    assert(p12 == p12b)

    assert(p1 != p12)
    assert(p1 < p12)

    assert(p12 != p1)
    assert(p12 > p1)

    assert(p12 != p13)
    assert(p12 < p13)
  }

}
