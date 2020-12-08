import RichClasses.SharedValues
import org.scalatest.FunSuite
import third.TobogganSlopeProblem

/** Tests around day 3 excercises */
class Day3Suite extends FunSuite with SharedValues {

  private val tobogganSlopeProblem: TobogganSlopeProblem = new TobogganSlopeProblem()
  lazy val SOURCE_CONFIG_KEY = "thirdday.example.source.path" // reading from the example file
  lazy val defaultSteps: Seq[(Int, Int)] = tobogganSlopeProblem.calculateSteps(currentPosition = (0, 10)) // the steps from PART1

  test("The correct number of obstacles should be calculated given example input") {
    assert(tobogganSlopeProblem.howManyObstaclesWillIHit(readerPath = DEFAULT_FILE_PATH, steps = defaultSteps) == 7, "Wrong answer calculated")
  }

  test("The correct number of obstacles should be calculated for each of the given ops using example input") {
    val expectedOutput: List[Long] = List(2,7,3,4,2)
    val expectedCalculation = multipleByEach(expectedOutput)
    val numberOfObstaclesSequence = tobogganSlopeProblem.howManyObstaclesGivenManySteps(readerPath = DEFAULT_FILE_PATH)
    assert(numberOfObstaclesSequence.toSet == expectedOutput.toSet)
    assert(expectedCalculation == multipleByEach(numberOfObstaclesSequence.map(_.toLong).toList))
  }

  test("The correct number of obstacles should be calculated given test input") {
    val startingCoordinates = (0, 322)
    val steps = tobogganSlopeProblem.calculateSteps(startingCoordinates)
    assert(tobogganSlopeProblem.howManyObstaclesWillIHit(maxCoordinates = (30, 322), startingCoordinates = startingCoordinates, steps = steps) == 280)
  }

  test("The correct number of obstacles should be calculated for each of the given ops using test input") {
    val startingCoordinates = (0, 322)
    val numberOfObstaclesSequence = tobogganSlopeProblem.howManyObstaclesGivenManySteps(maxCoordinates = (30, 322), startingCoordinates = startingCoordinates)
    println(numberOfObstaclesSequence)
    println(multipleByEach(numberOfObstaclesSequence.map(_.toLong).toList))

  }

}
