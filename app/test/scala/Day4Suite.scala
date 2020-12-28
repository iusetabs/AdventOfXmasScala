package scala

import RichClasses.SharedValues
import main.scala.forth.ValidPassports
import org.scalatest.FunSuite

class Day4Suite extends FunSuite with SharedValues {

  private val validPassports: ValidPassports = new ValidPassports()
  lazy val SOURCE_CONFIG_KEY = "forthday.example.source.path" // reading from the example file
  lazy val SOURCE_CONFIG_PART2_KEY = "forthday.part2.example.source.path"

  test("PART1: Invalid passport should be detected") {
    val invalidPassportMetadata = "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929"
    assert(validPassports.passportIsInvalid(invalidPassportMetadata, isPart1 = true))
  }

  test("PART1: Valid passport should be detected") {
    val validPassportMetadata = "ecl:gry pid:860033327 eyr:2020 hcl:#fffff byr:1937 iyr:2017 cid:147 hgt:183cm"
    assert(validPassports.passportIsValid(validPassportMetadata, isPart1 = true))
  }

  test("PART1: Given example input 2 valid passports should be returned") {
    val finalReport = validPassports.howManyPassportsAreValid(defaultReader.getLines)
    println(finalReport.toString)
    assert(finalReport.validPassports == 2)
  }

  test("PART1: Given excercise input correct answer should be returned") {
    val finalReport = validPassports.howManyPassportsAreValid(getReader(getStringFromConfig(validPassports.SOURCE_CONFIG_KEY)).getLines)
    println(finalReport.toString)
  }

  test("regex for hair colour should not match invalid strings") {
    val regex = """^([0-9]|[a-f]){6}$""".r
    val invalidStrings = Seq("123abz", "123abcd", "abcd", "", "0", "abcdefg")
    invalidStrings.foreach {
      value =>
        val maybeFound = regex.findFirstIn(value)
        assert(maybeFound.isEmpty)
    }
  }

  test("regex for hair colour should match valid strings") {
    val regex = """^([0-9]|[a-f]){6}$""".r
    val validStrings = Seq("123abc", "123456", "abcdef", "a97842")
    validStrings.foreach {
      value =>
        println(value)
        val maybeFound = regex.findFirstIn(value)
        assert(maybeFound.isDefined)
    }
  }

  test("PART2: Should be 4 invalid, 4 valid") {
    val finalReport = validPassports.howManyPassportsAreValid(getReader(getStringFromConfig(SOURCE_CONFIG_PART2_KEY)).getLines, isPart1 = false)
    println(finalReport.toString)
  }

  test("PART2: Given excercise input correct answer should be returned") {
    val finalReport = validPassports.howManyPassportsAreValid(getReader(getStringFromConfig(validPassports.SOURCE_CONFIG_KEY)).getLines, isPart1 = false)
    println(finalReport.toString)
  }

}
