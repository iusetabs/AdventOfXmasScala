package seventh

import RichClasses.SharedValues

import scala.annotation.tailrec
import scala.io.BufferedSource

class LuggageProblem extends SharedValues {
  val SOURCE_CONFIG_KEY: String = "seventhday.source.path"

  //shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
  def parseRule(ruleString: String): BagRule = {
    val bagColour = ruleString.trim.split("contain").head.trim.split(" ").dropRight(1).mkString(" ")
    val lastPartOfRule = ruleString.split("contain").last
    val containingColoursOfCount = if ( lastPartOfRule.trim == "no other bags." ) {
      Map.empty[String, Int]
    } else {
      val numberRegex = """[0-9]""".r
      lastPartOfRule.split(",").map {
        colourAndCount =>
          val count = numberRegex.findFirstIn(colourAndCount).get
          val colour = colourAndCount.trim.split(" ").drop(1).dropRight(1).mkString(" ")
          (colour, count.toInt)
      }.toMap
    }
    BagRule(bagColour, containingColoursOfCount)
  }

  /*
    A bright white bag, which can hold your shiny gold bag directly.
    A muted yellow bag, which can hold your shiny gold bag directly, plus some other bags.
    A dark orange bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
    A light red bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
 */
  @tailrec
  final def getBagsThatCanContainColour(iter: Iterator[String], bagColour: String, coloursThatCanContain: List[BagRule]): List[BagRule] = {
    if ( iter.isEmpty ) {
      coloursThatCanContain
    } else {
      val rule = parseRule(iter.next())
      val updatedColoursThatCanContain = if ( rule.containingColoursOfCount.keys.toList.contains(bagColour) ) {
        coloursThatCanContain :+ rule
      } else {
        coloursThatCanContain
      }
      getBagsThatCanContainColour(iter, bagColour, updatedColoursThatCanContain)
    }
  }

  @tailrec
  final def getCountOfBagsThatCanEventuallyContainAColour(source: BufferedSource, bagColours: List[String], bagColoursAlreadyChecked: Set[BagRule]): Set[BagRule] = {
    if ( bagColours.isEmpty ) {
      bagColoursAlreadyChecked
    } else {
      val iter = source.reset.getLines
      val colour = bagColours.head
      val coloursThatCanContainDesiredColour = getBagsThatCanContainColour(iter, colour, List.empty).filterNot(rule => bagColoursAlreadyChecked.map(_.bagColour).contains(rule.bagColour))
      val updatedBagColours = bagColours.drop(1) ++ coloursThatCanContainDesiredColour.map(_.bagColour)
      val updatedBagColoursAlreadyChecked = bagColoursAlreadyChecked ++ coloursThatCanContainDesiredColour
      getCountOfBagsThatCanEventuallyContainAColour(source, updatedBagColours, updatedBagColoursAlreadyChecked)
    }
  }

  @tailrec
  final def getAllBagColoursWithinColour(source: BufferedSource, bagColours: List[String], bagColoursAlreadyChecked: Set[BagRule]): Set[BagRule] = {
    if ( bagColours.isEmpty ) {
      bagColoursAlreadyChecked
    } else {
      val iter = source.reset.getLines
      val colour = bagColours.head
      val rule = parseRule(iter.find(line => parseRule(line).bagColour == colour).get)
      val updatedBagColoursAlreadyChecked = bagColoursAlreadyChecked + rule
      val updatedBagColours = bagColours.drop(1) ++ rule.containingColoursOfCount.keys.toList.filterNot(bagColoursAlreadyChecked.map(_.bagColour).contains)
      getAllBagColoursWithinColour(source, updatedBagColours, updatedBagColoursAlreadyChecked)
    }
  }

  final def getCountOfBags(mainBagColour: String, allRules: Set[BagRule]): Int = {
    val rule = allRules.find(_.bagColour == mainBagColour).get
    if ( rule.containingColoursOfCount.isEmpty) {
      1
    } else {
     val sum = rule.containingColoursOfCount.map {
        case (colour, quantity) =>
          val countInBagsBelow = getCountOfBags(colour, allRules)
          countInBagsBelow * quantity
      }.sum
      sum
    }
  }
}

case class BagRule(bagColour: String, containingColoursOfCount: Map[String, Int])
