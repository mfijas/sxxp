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

import org.scalatest.FunSuite
import scala.xml.{NodeSeq, Node, XML}
import org.sxxp.xpath1.parser.types.XNodeSeq
import org.sxxp.xpath1.parser.XPathParser
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.xpath.{XPathConstants, XPathFactory}
import org.w3c.dom.NodeList
import org.w3c.dom
import javax.xml.transform.TransformerFactory
import java.io.StringWriter
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult


class JavaComparisonTests extends FunSuite {

  val parser: XPathParser = new XPathParser

  def loadScalaXml(fileName: String) =
    XML.load(getClass.getClassLoader.getResourceAsStream(fileName))

  def evaluate(node: Node, path: String) =
    Evaluator.evaluate(node, parser.parsePathExpression(path)) match {
      case XNodeSeq(nodeSeq) => nodeSeq
      case _ => fail("XNodeSeq expected when evaluating PathExpression")
    }

  def loadJavaXml(fileName: String) =
    DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(getClass.getClassLoader.getResourceAsStream(fileName))

  def evaluateJavaXPath(document: dom.Document, path: String) = {
    val expr = XPathFactory.newInstance.newXPath.compile(path)
    val node = document.getDocumentElement
    expr.evaluate(node, XPathConstants.NODESET)
  }

  def serializeScalaNode(node: Node) = node.toString.replace("\r\n", "\n")

  def serializeJavaNode(node: dom.Node) = {
    val transformer = TransformerFactory.newInstance.newTransformer
    transformer.setOutputProperty("omit-xml-declaration", "yes")
    val writer = new StringWriter
    transformer.transform(new DOMSource(node), new StreamResult(writer))
    writer.toString.replace("\r\n", "\n")
  }

  def compareNodeSeqToNodeList(seq: NodeSeq, list: NodeList, description: String) = {
    assert(seq.size === list.getLength, description)
    for (i <- 0 until seq.size - 1) {
      val scalaNode = seq(i)
      val javaNode = list.item(i)
      assert(serializeScalaNode(scalaNode) === serializeJavaNode(javaNode), description)
    }
  }

  def compareResult(xmlFileName: String, path: String) {
    val scalaXml = loadScalaXml(xmlFileName)
    val scalaResult = evaluate(scalaXml, path)

    val javaXml = loadJavaXml(xmlFileName)
    val javaResult = evaluateJavaXPath(javaXml, path).asInstanceOf[org.w3c.dom.NodeList]

    compareNodeSeqToNodeList(scalaResult, javaResult, s"for path '$path'")
  }

  test("simple path") {
    compareResult("test.xml", "a")
    compareResult("test.xml", "a/aa")
    compareResult("test.xml", "/root/a")
    compareResult("test.xml", "/root//aa")
    compareResult("test.xml", "/root//aa[2]")
    compareResult("test.xml", "//aa[1]")
    compareResult("test.xml", "/*")
    compareResult("test.xml", "a//.")
    compareResult("test.xml", "a[1]/b[. = 'abcd']")
    compareResult("test.xml", "numbers/n[. = 234]")
    compareResult("test.xml", "numbers/n[. = 1]")
    compareResult("test.xml", "numbers/n[. = true]")
    compareResult("test.xml", "numbers/n[. = false]")
    compareResult("test.xml", "numbers/n[number(.) = 234]")
    compareResult("test.xml", "numbers/n[boolean(.)]")
    compareResult("test.xml", "numbers/n[string(.) = '234']")
    // would throw IllegalStateException
    // compareResult("test.xml", "numbers/n[illegalFun('abc',1)]")
  }

  test("root selector") {
    // write a better comparison to distinguish between java selecting document
    // root and scala selecting document element
    compareResult("test.xml", "/")
  }
}
