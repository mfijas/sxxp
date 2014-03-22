package org.sxxp.xpath1.parser.types

import org.scalatest.FlatSpec
import org.sxxp.xpath1.parser.axis.NodeWithAncestors

class NodeWithAncestorsSeqTest extends FlatSpec {

  "NodeWithAncestorsSeq" should "extract text value from nodes" in {

    // given
    val xml =
      <root>
        <a>aaa</a>
        <b>bbb</b>
        <c>ccc</c>
      </root>
    val a = NodeWithAncestors((xml \ "a").head, List(xml))
    val b = NodeWithAncestors((xml \ "b").head, List(xml))
    val c = NodeWithAncestors((xml \ "c").head, List(xml))
    val seq = Seq(a, b, c)

    // when
    val extractedText = NodeWithAncestorsSeq.extractText(seq)

    // then
    assert(extractedText === "aaabbbccc")
  }

}
