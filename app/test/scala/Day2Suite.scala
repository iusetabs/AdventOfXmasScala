import org.scalatest.FunSuite
import second.{PasswordCriteria, PasswordValidityChecker}

/** Tests around code for Day2 */
class Day2Suite extends FunSuite {

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
        assert(PasswordValidityChecker.isPasswordValid(password, criteria),
          s"Password $password was valid! Should be invalid according to criteria $criteria")
    }

  }

  test("invalud passwords from example should be marked as invalid by system") {
    val invalidPasswords = Seq("cdefg")
    val inputInScope = inputFromExcercise.filter{ case (password, _) => invalidPasswords.contains(password)}
    inputInScope.foreach {
      case (password, criteria) =>
        assert(PasswordValidityChecker.isPasswordInvalid(password, criteria),
          s"Password $password was valid! Should be invalid according to criteria $criteria")
    }
  }
}
