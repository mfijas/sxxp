import de.johoop.jacoco4sbt._
import JacocoPlugin._

name := "sxxp"

organization := "org.sxxp"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.0" % "test",
  "org.slf4j" % "slf4j-simple" % "1.7.6"
)

jacoco.settings

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-u", "%s" format (target.value / "../shippable/testresults"))

jacoco.outputDirectory in jacoco.Config := crossTarget.value / "../../shippable/codecoverage"

jacoco.reportFormats in jacoco.Config := Seq(
  XMLReport(encoding = "utf-8"),
  ScalaHTMLReport(withBranchCoverage = true))
