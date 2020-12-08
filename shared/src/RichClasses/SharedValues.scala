package RichClasses

import helpers.CommonHelper

import scala.io.{BufferedSource, Source}

trait SharedValues {

  val SOURCE_CONFIG_KEY: String

  final lazy val DEFAULT_FILE_PATH: String = CommonHelper.getStringFromConf(SOURCE_CONFIG_KEY)
  final lazy val defaultReader: BufferedSource = getReader(DEFAULT_FILE_PATH)

  def getReader(filePath: String): BufferedSource = Source.fromFile(filePath)




}
