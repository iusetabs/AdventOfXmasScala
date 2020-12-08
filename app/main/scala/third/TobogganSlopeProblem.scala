package third

import RichClasses.SharedValues
import scala.annotation.tailrec
import scala.io.BufferedSource

class TobogganSlopeProblem extends SharedValues {

  lazy val SOURCE_CONFIG_KEY = "thirdday.source.path"
  lazy val listOfOpsFromPart2 = Seq((1,-1), (3,-1), (5,-1), (7,-1), (1,-2))

  /** Assuming a downward trajectory to the right */
  @tailrec
  final def calculateSteps(currentPosition: (Int, Int), listOfPreviousSteps: Seq[(Int, Int)] = Seq.empty, changeToX: Int = 3, changeToY: Int = -1): Seq[(Int, Int)] = {
    val (currentXCoordinate, currentYCoordiante) = currentPosition
    if ( currentYCoordiante < 0 ) {
      listOfPreviousSteps
    } else {
      calculateSteps(currentPosition.copy(currentXCoordinate + changeToX, currentYCoordiante + changeToY), listOfPreviousSteps :+ currentPosition, changeToX = changeToX, changeToY = changeToY)
    }
  }

  /** The X coordinate may be greater that our current field of vision */
  def correctXCoordinate(currentXCoordinate: Int, maxXCoordiante: Int): Int = {
    val indexResetsToZeroAtIndex = maxXCoordiante+1
    currentXCoordinate - (((currentXCoordinate / indexResetsToZeroAtIndex) * indexResetsToZeroAtIndex))
  }

  def howManyObstaclesGivenManySteps(readerPath: String = DEFAULT_FILE_PATH, charsWhichAreObstacles: Seq[Char] = Seq('#'),
                               startingCoordinates: (Int, Int) = (0, 10), maxCoordinates: (Int, Int) = (10, 10), listOfOperations: Seq[(Int, Int)] = listOfOpsFromPart2): Seq[Int] = {
    listOfOperations.map {
      case (changeToX, changeToY) =>
        val steps = calculateSteps(startingCoordinates, changeToX = changeToX, changeToY = changeToY)
        howManyObstaclesWillIHit(readerPath, charsWhichAreObstacles, startingCoordinates, maxCoordinates, steps)
    }

  }

  def howManyObstaclesWillIHit(readerPath: String = DEFAULT_FILE_PATH, charsWhichAreObstacles: Seq[Char] = Seq('#'),
                               startingCoordinates: (Int, Int) = (0, 10), maxCoordinates: (Int, Int) = (10, 10), steps: Seq[(Int, Int)]): Int = {
    val reader = getReader(readerPath)
    val (_, startingYCoordinate) = startingCoordinates
    val (maxXCoordinate, maxYCoordinate) = maxCoordinates
    val countOfObstacles = reader.getLines.zipWithIndex.count {
      case (lineOfVision, index) if maxYCoordinate - index <= startingYCoordinate =>
        val currentYCoordinate = maxYCoordinate - index
        val maybeGrabCharAtIndex = steps.find {
          case (_, y) => y == currentYCoordinate
        }.map {
          case (x, _) => correctXCoordinate(x, maxXCoordinate)
        }
        maybeGrabCharAtIndex.exists {
          grabCharAtIndex =>
            val char = lineOfVision.lift(grabCharAtIndex).get
            charsWhichAreObstacles.contains(char)
        }
      case (_, _ ) => false
    }
    reader.close()
    countOfObstacles
  }
}
