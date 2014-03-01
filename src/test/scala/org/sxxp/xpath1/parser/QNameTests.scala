package org.sxxp.xpath1.parser

import org.scalatest.FlatSpec

class QNameTests extends FlatSpec {

  "QName.getFullName" should "return fully qualified name for qnames with namespace specified" in {

    // given
    val ns = "http://schemas.sxxp.org/s"
    val prefix = "s"
    val localPart = "someName"
    val qname = QName(ns, prefix, localPart)

    // when
    val fullName = qname.getFullName

    // then
    assert(fullName === "{" + ns + "}" + localPart)
  }

}
