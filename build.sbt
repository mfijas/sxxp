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

testOptions in Test <+= (target in Test) map {
  t => Tests.Argument(TestFrameworks.ScalaTest, "junitxml(directory=\"%s\")" format (t / "../shippable/testresults"))
}
