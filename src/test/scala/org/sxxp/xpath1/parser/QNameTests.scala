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
