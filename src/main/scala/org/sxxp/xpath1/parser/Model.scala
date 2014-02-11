package org.sxxp.xpath1.parser

case class QName(prefix: String, localPart: String)

object QName {
  def apply(localPart: String): QName = QName("", localPart)
}

sealed trait NodeTest

case class NameNodeTest(name: QName) extends NodeTest

case class NodeTypeTest(nodeType: NodeType) extends NodeTest

sealed trait NodeType

case object CommentNodeType extends NodeType

case object TextNodeType extends NodeType

case object ProcessingInstructionNodeType extends NodeType

case object NodeNodeType extends NodeType


//case class AxisSpecifier(axis: String)

case class Predicate(expr: Expr)

sealed trait LocationPath {
  val steps: List[Step]

  def :+(step: Step): LocationPath
}

case class RelativeLocationPath(steps: List[Step]) extends LocationPath {
  def :+(step: Step) = RelativeLocationPath(steps :+ step)
}

object RelativeLocationPath {
  def apply(step: Step): RelativeLocationPath = RelativeLocationPath(List(step))
}

/**
 * Location path starting with "/"
 */
case class AbsoluteLocationPath(steps: List[Step]) extends LocationPath {
  override def :+(step: Step): LocationPath = AbsoluteLocationPath(steps :+ step)
}

object AbsoluteLocationPath {
  def apply(path: LocationPath): AbsoluteLocationPath = AbsoluteLocationPath(path.steps)
}

/**
 * Location path starting with "//"
 */
case class AbbreviatedAbsoluteLocationPath(steps: List[Step]) extends LocationPath {
  override def :+(step: Step): LocationPath = AbbreviatedAbsoluteLocationPath(steps :+ step)
}

object AbbreviatedAbsoluteLocationPath {
  def apply(path: LocationPath): AbbreviatedAbsoluteLocationPath = AbbreviatedAbsoluteLocationPath(path.steps)
}

/**
 * Root element location path: "/"
 */
case object RootLocationPath extends LocationPath {
  override val steps: List[Step] = List.empty

  override def :+(step: Step): LocationPath = throw new IllegalStateException("cannot add step to root location")
}


sealed trait Step

/**
 * "."
 */
case object CurNodeStep extends Step

/**
 * ".."
 */
case object ParentNodeStep extends Step

case class NodeStep(nodeTest: NodeTest, predicates: List[Predicate]) extends Step

/**
 * //
 */
case class AbbreviatedNodeStep(step: Step) extends Step