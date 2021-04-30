package scala

import RichClasses.SharedValues
import org.scalatest.FunSuite
import seventh.{BagRule, LuggageProblem}

/** test functions around day 7 tasks */
class Day7Suite extends FunSuite with SharedValues {
  val SOURCE_CONFIG_KEY: String = "seventhday.example.source.path"
  val SOURCE_CONFIG_KEY_PART2: String = "seventhday.example2.source.path"
  val luggageProblem = new LuggageProblem()
  val expectedPart1OutputFromExample = List("bright white", "muted yellow", "dark orange", "light red")

  test("PART1: Rule with multiple colours should be parsed correctly") {
    val exampleRule = "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
    val expectedOutput = BagRule("shiny gold", Map("dark olive" -> 1, "vibrant plum" -> 2))
    assert(luggageProblem.parseRule(exampleRule) == expectedOutput)
  }

  test("PART1: Rule with empty bags should be parsed correctly") {
    val exampleRule = "dotted black bags contain no other bags."
    val expectedOutput = BagRule("dotted black", Map.empty)
    assert(luggageProblem.parseRule(exampleRule) == expectedOutput)
  }

  /** A bright white bag, which can hold your shiny gold bag directly.
    A muted yellow bag, which can hold your shiny gold bag directly, plus some other bags. */
  test("PART1: Given example input and a shiny gold bag, the other bag colours that contain shiny gold bags should amount to 2 colours") {
    val colours = luggageProblem.getBagsThatCanContainColour(defaultReader.getLines, "shiny gold", List.empty)
    println(colours)
    assert(colours.length==2)
  }

  test("PART1: Given example input the answer should be 4") {
    val count = luggageProblem.getCountOfBagsThatCanEventuallyContainAColour(defaultReader, List("shiny gold"), Set.empty).size
    println(count)
    assert(count == 4)
  }

  test("PART1: Given exercise input, print the answer") {
    val reader = getReader(getStringFromConfig(luggageProblem.SOURCE_CONFIG_KEY))
    val count = luggageProblem.getCountOfBagsThatCanEventuallyContainAColour(reader, List("shiny gold"), Set.empty).size
    println(count)
  }

  test("PART2: Given part1 example input and shiny gold bag, 32 bags should be returned") {
    val colours = luggageProblem.getAllBagColoursWithinColour(defaultReader, List("shiny gold"), Set.empty)
    val count = luggageProblem.getCountOfBags("shiny gold", colours)
    assert(count == 32)
  }

  test("PART2: Given part2 example input and shiny gold bag, 126 bags should be returned") {
    val reader = getReader(getStringFromConfig(SOURCE_CONFIG_KEY_PART2))
    val colours = luggageProblem.getAllBagColoursWithinColour(reader, List("shiny gold"), Set.empty)
    val count = luggageProblem.getCountOfBags("shiny gold", colours)
    val countInTopBag = colours.find(_.bagColour == "shiny gold").get.containingColoursOfCount.values.sum
    assert(count+countInTopBag == 126)
  }

}
