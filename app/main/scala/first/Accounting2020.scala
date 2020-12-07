package first

import helpers.CommonHelper
import scala.annotation.tailrec
import scala.io.Source

object Accounting2020 extends App {

  lazy val DAY_1_INPUT_FILE_PATH: String = "firstday.source.path"

  def inputValues(targetNumber: Long, inputFilePath: String = DAY_1_INPUT_FILE_PATH): List[Long] = {
    val inputFile = CommonHelper.getStringFromConf(inputFilePath)
    val reader = Source.fromFile(inputFile)
    val values = reader.getLines.flatMap(_.toLongOption).filter(_ < targetNumber).toList
    reader.close()
    values
  }

  @tailrec
  def multipleByEach(seq: List[Long], multiple: Long = 1): Long = {
    seq match {
      case a :: b :: tail =>
        multipleByEach(tail, multiple*a*b)
      case a :: tail =>
        multipleByEach(tail, multiple*a)
      case _ =>  multiple
    }
  }

  def findValuesThatAddToX(values: List[Long], desiredTotal: Long, howManyNumbersToAdd: Int): List[Long] = {
    values.combinations(howManyNumbersToAdd).find(combination => combination.sum == desiredTotal).getOrElse(List.empty[Long])
  }

  def execute(targetNumber: Int = 2020, inputFilePath: String = DAY_1_INPUT_FILE_PATH, desiredTotal: Long = 2020, howManyNumbersToAdd: Int): Seq[Long] = {
    val values = inputValues(targetNumber, inputFilePath)
    val figuresFromAccounting = findValuesThatAddToX(values, desiredTotal, howManyNumbersToAdd)
    println(s"The values that add to the desired output $targetNumber, are ${figuresFromAccounting.dropRight(1).map(num => s"$num and")} ${figuresFromAccounting.last}")
    println(s"${figuresFromAccounting.dropRight(1).foreach(num => s"$num X")} ${figuresFromAccounting.last}=$figuresFromAccounting = ${multipleByEach(figuresFromAccounting)}")
    figuresFromAccounting
  }

}
