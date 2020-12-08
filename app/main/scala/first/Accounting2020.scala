package first

import RichClasses.SharedValues
import helpers.CommonHelper
import scala.io.Source

object Accounting2020 extends App with SharedValues {

  lazy val SOURCE_CONFIG_KEY: String = "firstday.source.path"

  def inputValues(targetNumber: Long, inputFilePath: String = SOURCE_CONFIG_KEY): List[Long] = {
    val inputFile = CommonHelper.getStringFromConf(inputFilePath)
    val reader = Source.fromFile(inputFile)
    val values = reader.getLines.flatMap(_.toLongOption).filter(_ < targetNumber).toList
    reader.close()
    values
  }

  def findValuesThatAddToX(values: List[Long], desiredTotal: Long, howManyNumbersToAdd: Int): List[Long] = {
    values.combinations(howManyNumbersToAdd).find(combination => combination.sum == desiredTotal).getOrElse(List.empty[Long])
  }

  def execute(targetNumber: Int = 2020, inputFilePath: String = SOURCE_CONFIG_KEY, desiredTotal: Long = 2020, howManyNumbersToAdd: Int): Seq[Long] = {
    val values = inputValues(targetNumber, inputFilePath)
    val figuresFromAccounting = findValuesThatAddToX(values, desiredTotal, howManyNumbersToAdd)
    println(s"The values that add to the desired output $targetNumber, are ${figuresFromAccounting.dropRight(1).map(num => s"$num and")} ${figuresFromAccounting.last}")
    println(s"${figuresFromAccounting.dropRight(1).foreach(num => s"$num X")} ${figuresFromAccounting.last}=$figuresFromAccounting = ${multipleByEach(figuresFromAccounting)}")
    figuresFromAccounting
  }

}
