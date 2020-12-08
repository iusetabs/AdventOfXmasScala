package second

import helpers.CommonHelper
import scala.io.Source

/** To return how many passwords are valid */
object PasswordValidityChecker extends App {

  val DAY_2_SOURCE_CONFIG_KEY: String = "secondday.source.path"

  def getValuesFromInput(targetNumber: Long, inputFilePath: String = DAY_2_SOURCE_CONFIG_KEY): List[Long] = {
    val inputFile = CommonHelper.getStringFromConf(inputFilePath)
    val reader = Source.fromFile(inputFile)
    val values = reader.getLines.flatMap(_.toLongOption).filter(_ < targetNumber).toList
    // EXAMPLE: 8-11 l: qllllqllklhlvtl
    reader.close()
    values
  }

  /** Check password validity */
  def isPasswordValid(password: String, criteria: PasswordCriteria): Boolean = {
    val requiredCharacterCount = password.filter(_ == criteria.requiredCharacter).length
    requiredCharacterCount >= criteria.minimumCharacterCount && requiredCharacterCount <= criteria.maximumCharacterCount
  }

  def isPasswordInvalid(password: String, criteria: PasswordCriteria): Boolean = {
    !isPasswordValid(password, criteria)
  }

}

/** @param minimumCharacterCount: The minimum number of times a required character needs to appear in a password
 *  @param maximumCharacterCount: The maximum number of times a required character needs to appear in a password */
case class PasswordCriteria(minimumCharacterCount: Int, maximumCharacterCount: Int, requiredCharacter: Char)
