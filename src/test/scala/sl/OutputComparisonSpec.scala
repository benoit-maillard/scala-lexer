package sl

import org.scalatest._
import java.io.File
import scala.io.Source

import scala.reflect.classTag


abstract class OutputComparisonSpec extends FlatSpec {
  val inputExtension: String
  val outputExtension: String
  val pipeline: String => String

  val rootDir = "src/test/resources/"
  val rootInput = rootDir + "input/"
  val rootOutput = rootDir + "output/"

  def outputMatch(testName: String): Assertion = {
    val expected = input(testName)

    val actual = output(testName)

    assertResult(expected)(actual)
  }

  def output(testName: String): String = {
    val inputFileName = rootInput + testName + inputExtension
    pipeline(inputFileName)
  }

  def input(testName: String): String =
    Source.fromFile(rootOutput + testName + outputExtension)
      .getLines().mkString
}