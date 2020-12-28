package scala

import RichClasses.SharedValues
import org.scalatest.FunSuite
import sixth.CustomsForm

/** Tests around sixth day tasks */
class Day6Suite extends FunSuite with SharedValues {
  val SOURCE_CONFIG_KEY: String = "sixthday.example.source.path"
  val customsForm = new CustomsForm

  test("Given example input, correct combined positive answers count should be returned") {
    assert(customsForm.countCombinedPositiveAnswers(defaultReader.getLines, isPart1 = true) == 11)
  }

  test("Given excercise input, correct combined positive answers count should be returned"){
    val reader = getReader(getStringFromConfig(customsForm.SOURCE_CONFIG_KEY)).getLines
    println(customsForm.countCombinedPositiveAnswers(reader, isPart1 = true))
  }

  test("PART2: Given example input, correct combined positive answers count should be returned") {
    assert(customsForm.countCombinedPositiveAnswers(defaultReader.getLines, isPart1 = false) == 6)
  }

  test("PART2: Given excercise input, correct combined positive answers count should be returned"){
    val reader = getReader(getStringFromConfig(customsForm.SOURCE_CONFIG_KEY)).getLines
    println(customsForm.countCombinedPositiveAnswers(reader, isPart1 = false))
  }
}
