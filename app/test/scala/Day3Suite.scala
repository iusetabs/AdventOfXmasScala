import RichClasses.SharedValues
import org.scalatest.FunSuite
import third.TobogganSlopeProblem

/** Tests around day 3 excercises */
class Day3Suite extends FunSuite with SharedValues {

  private val tobogganSlopeProblem: TobogganSlopeProblem = new TobogganSlopeProblem()
  lazy val SOURCE_CONFIG_KEY = "thirdday.example.source.path" // reading from the example file

  test("The correct number of obstacles should be calculated given example") {
    assert(tobogganSlopeProblem.howManyObstaclesWillIHit(reader = defaultReader) == 7, "Wrong answer calculated")
  }

  test("The correct number of obstacles should be calculated") {
    println(tobogganSlopeProblem.howManyObstaclesWillIHit(maxCoordinates = (30, 322), startingCoordinates = (0, 322)))
  }

}
