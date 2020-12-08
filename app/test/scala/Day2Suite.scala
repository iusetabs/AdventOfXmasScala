import org.scalatest.FunSuite
import second.{PasswordCriteria, PasswordValidityChecker}

/** Tests around code for Day2 */
class Day2Suite extends FunSuite {

  val passwordValidityChecker : PasswordValidityChecker = new PasswordValidityChecker()

  val inputFromExcercise: Seq[(String, PasswordCriteria)] = Seq(
    ("abcde", PasswordCriteria(1, 3, 'a')),
    ("cdefg", PasswordCriteria(1, 3, 'b')),
    ("ccccccccc", PasswordCriteria(2, 9, 'c'))
  )

  test("valid passwords from example should be marked as valid by system") {
    val validPasswords = Seq("abcde", "ccccccccc")
    val inputInScope = inputFromExcercise.filter{ case (password, _) => validPasswords.contains(password)}
    inputInScope.foreach {
      case (password, criteria) =>
        assert(passwordValidityChecker.isPasswordValid(password, criteria),
          s"Password $password was valid! Should be invalid according to criteria $criteria")
    }

  }

  test("invalid passwords from example should be marked as invalid by system") {
    val invalidPasswords = Seq("cdefg")
    val inputInScope = inputFromExcercise.filter{ case (password, _) => invalidPasswords.contains(password)}
    inputInScope.foreach {
      case (password, criteria) =>
        assert(passwordValidityChecker.isPasswordInvalid(password, criteria),
          s"Password $password was valid! Should be invalid according to criteria $criteria")
    }
  }

  test("PART1: How many valid passwords are there in the given input") {
    val countOfValidPasswordsFromDefaultFile = passwordValidityChecker.howManyValidPasswordsFromFile()
    println(countOfValidPasswordsFromDefaultFile)
  }
}