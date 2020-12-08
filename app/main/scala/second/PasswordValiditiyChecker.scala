package second

import helpers.CommonHelper
import scala.io.Source

/** To return how many passwords are valid */
class PasswordValidityChecker extends App {

  lazy val DAY_2_SOURCE_CONFIG_KEY: String = "secondday.source.path"
  lazy val DEFAULT_FILE_PATH: String = CommonHelper.getStringFromConf(DAY_2_SOURCE_CONFIG_KEY)

  /** Parse a line according to example input: 8-11 l: qllllqllklhlvtl
   *  Returns a tuple with the first element being the password and the second the criteria */
  def parseLine(line: String): (String, PasswordCriteria) = {
    val maybePassword = line.split(" ").lastOption
    val maybeRange = line.split(" ").headOption
    val maybeChar = line.split(" ").lift(1).flatMap(_.headOption)
    val maybeMinimum = maybeRange.flatMap(range => range.split("-").headOption.flatMap(_.toIntOption))
    val maybeMaximum = maybeRange.flatMap(range => range.split("-").lastOption.flatMap(_.toIntOption))

    if (maybePassword.isEmpty || maybeRange.isEmpty || maybeChar.isEmpty || maybeMinimum.isEmpty || maybeMaximum.isEmpty) {
      throw new Exception(s"Invalid input detected, line=$line, maybePassword=$maybePassword, maybeChar=$maybeChar, maybeRange=$maybeRange")
    } else {
      val criteria = PasswordCriteria(maybeMinimum.get, maybeMaximum.get, maybeChar.get)
      val password = maybePassword.get
      (password, criteria)
    }
  }

  def howManyValidPasswordsFromFile(filePath: String = DEFAULT_FILE_PATH): Int = {
    val reader = Source.fromFile(filePath)
    val countOfValidPasswords = reader.getLines.count {
      line =>
          val (password, criteria) = parseLine(line)
          isPasswordValidPart1(password, criteria)
    }
    reader.close()
    countOfValidPasswords
  }

  /** Check password validity PART1 */
  def isPasswordValidPart1(password: String, criteria: PasswordCriteria): Boolean = {
    val requiredCharacterCount = password.filter(_ == criteria.requiredCharacter).length
    requiredCharacterCount >= criteria.minimumCharacterCount && requiredCharacterCount <= criteria.maximumCharacterCount
  }

  /** Check password validity PART2 */
  def isPasswordValidPart2(password: String, criteria: PasswordCriteria): Boolean = {
    val isCharARequiredChar = password.lift(criteria.firstIndex).contains(criteria.requiredCharacter)
    val isCharBRequiredChar = password.lift(criteria.secondIndex).contains(criteria.requiredCharacter)
    isCharARequiredChar && !isCharBRequiredChar || !isCharARequiredChar && isCharBRequiredChar

  }

  /** For code readability, a negation of the valid implementation */
  def isPasswordInvalidPart1(password: String, criteria: PasswordCriteria): Boolean = {
    !isPasswordValidPart1(password, criteria)
  }

  /** For code readability, a negation of the valid implementation */
  def isPasswordInvalidPart2(password: String, criteria: PasswordCriteria): Boolean = {
    !isPasswordValidPart2(password, criteria)
  }

}

/** @param minimumCharacterCount: The minimum number of times a required character needs to appear in a password
 *  @param maximumCharacterCount: The maximum number of times a required character needs to appear in a password */
case class PasswordCriteria(minimumCharacterCount: Int, maximumCharacterCount: Int, requiredCharacter: Char) {
  /** For PART2, without impacting PART1 implementation, denotes the first index to inspect */
  def firstIndex = minimumCharacterCount - 1

  /** For PART2, without impacting PART1 implementation, denotes the second index to inspect */
  def secondIndex = maximumCharacterCount - 1
}
