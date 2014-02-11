package org.sxxp.xpath1.parser

import org.scalatest.FlatSpec


class XPathParserTests extends FlatSpec {

  val parser = new XPathParser

  def verifyParseResult[T](production: parser.Parser[T], input: String, expected: Any) {
    val parseResult = parser.parseAll(production, input)
    parseResult match {
      case parser.Success(result, _) =>
        assert(result === expected)
      case parser.NoSuccess(msg, next) =>
        fail(msg)
    }
  }

  def verifyNameTestParseResult(input: String, expected: QName) = verifyParseResult(parser.nameTest, input, expected)

  def verifyNodeTestParseResult(input: String, expected: NodeTest) = verifyParseResult(parser.nodeTest, input, expected)

  def verifyPathExprParseResult(input: String, expected: Expr) = verifyParseResult(parser.pathExpr, input, expected)


  "nameTest" should "parse non-qualified name" in {
    verifyNameTestParseResult("someNode", QName("someNode"))
  }

  it should "parse qualified name" in {
    verifyNameTestParseResult("myns:someNode", QName("myns", "someNode"))
  }

  it should "parse any name (*)" in {
    verifyNameTestParseResult("*", QName("*"))
  }

  it should "parse any name for a given namespace (ns:*)" in {
    verifyNameTestParseResult("myns:*", QName("myns", "*"))
  }

  "nodeTest" should "parse qualified name" in {
    verifyNodeTestParseResult("myns:someNode", NameNodeTest(QName("myns", "someNode")))
  }

  "pathExpr" should "parse a" in {
    verifyPathExprParseResult("a",
      LocationPathExpr(
        RelativeLocationPath(
          List(
            NodeStep(NameNodeTest(QName("a")), List())))))
  }

  it should "parse a/b" in {
    verifyPathExprParseResult("a/b",
      LocationPathExpr(
        RelativeLocationPath(
          List(
            NodeStep(NameNodeTest(QName("a")), List()),
            NodeStep(NameNodeTest(QName("b")), List())))))
  }

  it should "parse a//b" in {
    verifyPathExprParseResult("a//b",
      LocationPathExpr(
        RelativeLocationPath(
          List(
            NodeStep(NameNodeTest(QName("a")), List()),
            AbbreviatedNodeStep(
              NodeStep(NameNodeTest(QName("b")), List()))))))
  }

  it should "parse /a/b" in {
    verifyPathExprParseResult("/a/b",
      LocationPathExpr(
        AbsoluteLocationPath(
          List(
            NodeStep(NameNodeTest(QName("a")), List()),
            NodeStep(NameNodeTest(QName("b")), List())))))
  }

  it should "parse //a/b" in {
    verifyPathExprParseResult("//a/b",
      LocationPathExpr(
        AbbreviatedAbsoluteLocationPath(
          List(
            NodeStep(NameNodeTest(QName("a")), List()),
            NodeStep(NameNodeTest(QName("b")), List())))))
  }

  it should "parse a[1]" in {
    verifyPathExprParseResult("a[1]",
      LocationPathExpr(
        RelativeLocationPath(
          List(
            NodeStep(NameNodeTest(QName("a")), List(Predicate(NumberExpr(1.0))))))))
  }

  it should "parse ." in {
    verifyPathExprParseResult(".",
      LocationPathExpr(
        RelativeLocationPath(
          List(CurNodeStep))))
  }

  it should "parse a[1]/b[. = 'abcd']" in {
    verifyPathExprParseResult("a[1]/b[.='abcd']",
      LocationPathExpr(
        RelativeLocationPath(
          List(
            NodeStep(NameNodeTest(QName("a")), List(Predicate(NumberExpr(1.0)))),
            NodeStep(NameNodeTest(QName("b")), List(Predicate(EqExpr(LocationPathExpr(RelativeLocationPath(List(CurNodeStep))), LiteralExpr("abcd"))))))))
    )
  }

  it should "parse a//." in {
    verifyPathExprParseResult("a//.",
      LocationPathExpr(RelativeLocationPath(List(NodeStep(NameNodeTest(QName("a")), List()), AbbreviatedNodeStep(CurNodeStep)))))
  }

}
