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

package org.sxxp.xpath1.exp

import org.scalatest.FlatSpec
import org.sxxp.xpath1.parser.XPathParser
import scala.xml.Node
import org.sxxp.xpath1.parser.types.XNodeSeq

class EvaluatorTest extends FlatSpec {

  val xml =
    <root>
      <a>
        <aa>someContent</aa>
        <aa>
          <aa/>
        </aa>
      </a>
      <a>
        <aa/>
      </a>
      <b>
        <aa/>
        <bb>
          <aa/>
        </bb>
      </b>
      <a></a>
    </root>

  val parser: XPathParser = new XPathParser()

  def evaluate(node: Node, path: String) =
    Evaluator.evaluate(node, parser.parsePathExpression(path)) match {
      case xNodeSeq: XNodeSeq => xNodeSeq.toNodeSeq
      case _ => fail("XNodeSeq expected when evaluating PathExpression")
    }

  "evaluate()" should "support simple node test step" in {
    // given
    val path = "a"
    val expected = xml \ "a"
    // when
    val result = evaluate(xml, path)
    // then
    assert(result === expected)
  }

  it should "select 'a/aa' path" in {
    // given
    val path = "a/aa"
    val expected = xml \ "a" \ "aa"
    // when
    val result = evaluate(xml, path)
    // then
    assert(result === expected)
  }

  it should "select '//aa' path" in {
    // given
    val path = "//aa"
    val expected = xml \\ "aa"
    // when
    val result = evaluate(xml, path)
    // then
    assert(result === expected)
  }

  it should "select '/root' path" in {
    // given
    val path = "/root"
    val expected = xml \\ "root"
    // when
    val result = evaluate(xml, path)
    // then
    assert(result === expected)
  }

  it should "select '/root//aa' path" in {
    // given
    val path = "/root//aa"
    val expected = xml \\ "aa"
    // when
    val result = evaluate(xml, path)
    // then
    assert(result === expected)
  }

  it should "select '//aa[1]' path" in {
    // given
    val path = "//aa[1]"
    // when
    val result = evaluate(xml, path)
    // then
    assert(result.size === 5)
  }

  it should "select '(//aa)[1]' path" in {
    // given
    val path = "(//aa)[1]"
    // when
    val result = evaluate(xml, path)
    // then
    assert(result.length === 1)
  }

  it should "select '/*' path" in {
    // given
    val path = "/*"
    val expected = xml \\ "root"
    // when
    val result = evaluate(xml, path)
    // then
    assert(result === expected, s"${result.getClass}, ${expected.getClass}")
  }


}
