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

  test("valid passwords from part1 example should be marked as valid by system") {
    val validPasswordsPart1 = Seq("abcde", "ccccccccc")
    val validPasswordsPart2 = Seq("abcde")
    // PART 1
    inputFromExcercise.filter{ case (password, _) => validPasswordsPart1.contains(password)}.foreach {
      case (password, criteria) =>
        assert(passwordValidityChecker.isPasswordValidPart1(password, criteria),
          s"Part1: Password $password was valid! Should be invalid according to criteria $criteria")
    }
    // PART 2
    inputFromExcercise.filter{ case (password, _) => validPasswordsPart2.contains(password)}.foreach {
      case (password, criteria) =>
        assert(passwordValidityChecker.isPasswordValidPart2(password, criteria),
          s"Part2: Password $password was valid! Should be invalid according to criteria $criteria")
    }


  }

  test("invalid passwords from part1 example should be marked as invalid by system") {
    val invalidPasswordsPart1 = Seq("cdefg")
    val invalidPasswordsPart2 = Seq("cdefg", "ccccccccc")
    // PART 1
    inputFromExcercise.filter{ case (password, _) => invalidPasswordsPart1.contains(password)}.foreach {
      case (password, criteria) =>
        assert(passwordValidityChecker.isPasswordInvalidPart1(password, criteria),
          s"Part1: Password $password was valid! Should be invalid according to criteria $criteria")
    }

    // PART 2
    inputFromExcercise.filter{ case (password, _) => invalidPasswordsPart2.contains(password)}.foreach {
      case (password, criteria) =>
        assert(passwordValidityChecker.isPasswordInvalidPart2(password, criteria),
          s"Part2: Password $password was valid! Should be invalid according to criteria $criteria")
    }
  }

  test("PART1: How many valid passwords are there in the given input") {
    val countOfValidPasswordsFromDefaultFile = passwordValidityChecker.howManyValidPasswordsFromFile()
    println(countOfValidPasswordsFromDefaultFile)
  }

  test("PART2: How many valid passwords are there in the given input") {
    val countOfValidPasswordsFromDefaultFile = passwordValidityChecker.howManyValidPasswordsFromFile(isForPart1 = false)
    println(countOfValidPasswordsFromDefaultFile)
  }
}
