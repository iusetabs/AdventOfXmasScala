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
          isPasswordValid(password, criteria)
    }
    reader.close()
    countOfValidPasswords
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
