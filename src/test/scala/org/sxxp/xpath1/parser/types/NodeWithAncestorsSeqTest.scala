package org.sxxp.xpath1.parser.types

import org.scalatest.FlatSpec
import org.sxxp.xpath1.parser.axis.{NodePath, NodeWithAncestors}

class NodeWithAncestorsSeqTest extends FlatSpec {

  "NodeWithAncestorsSeq" should "extract text value from nodes" in {

    // given
    val xml =
      <root>
        <a>aaa</a>
        <b>bbb</b>
        <c>ccc</c>
      </root>
    val a = NodeWithAncestors((xml \ "a").head, List(xml), NodePath(0))
    val b = NodeWithAncestors((xml \ "b").head, List(xml), NodePath(1))
    val c = NodeWithAncestors((xml \ "c").head, List(xml), NodePath(2))
    val seq = Seq(a, b, c)

    // when
    val extractedText = NodeWithAncestorsSeq.extractText(seq)

    // then
    assert(extractedText === "aaabbbccc")
  }

}
