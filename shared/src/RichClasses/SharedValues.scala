package RichClasses

import helpers.CommonHelper
import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

trait SharedValues {

  val SOURCE_CONFIG_KEY: String

  final lazy val DEFAULT_FILE_PATH: String = CommonHelper.getStringFromConf(SOURCE_CONFIG_KEY)
  final lazy val defaultReader: BufferedSource = getReader(DEFAULT_FILE_PATH)

  def getReader(filePath: String): BufferedSource = Source.fromFile(filePath)

  def getStringFromConfig(key: String): String = CommonHelper.getStringFromConf(key)

  @tailrec
  final def multipleByEach(seq: List[Long], multiple: Long = 1): Long = {
    seq match {
      case a :: b :: tail =>
        multipleByEach(tail, multiple*a*b)
      case a :: tail =>
        multipleByEach(tail, multiple*a)
      case _ =>  multiple
    }
  }




}
