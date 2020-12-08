package third

import RichClasses.SharedValues
import scala.annotation.tailrec
import scala.io.BufferedSource

class TobogganSlopeProblem extends SharedValues {

  lazy val SOURCE_CONFIG_KEY = "thirdday.source.path"

  /** Assuming a downward trajectory to the right */
  @tailrec
  final def calculateSteps(currentPosition: (Int, Int), listOfPreviousSteps: Seq[(Int, Int)] = Seq.empty, changeToX: Int = 3, changeToY: Int = -1): Seq[(Int, Int)] = {
    val (currentXCoordinate, currentYCoordiante) = currentPosition
    if ( currentYCoordiante < 0 ) {
      listOfPreviousSteps
    } else {
      calculateSteps(currentPosition.copy(currentXCoordinate + changeToX, currentYCoordiante + changeToY), listOfPreviousSteps :+ currentPosition)
    }
  }

  /** The X coordinate may be greater that our current field of vision */
  def correctXCoordinate(currentXCoordinate: Int, maxXCoordiante: Int): Int = {
    val indexResetsToZeroAtIndex = maxXCoordiante+1
    currentXCoordinate - (((currentXCoordinate / indexResetsToZeroAtIndex) * indexResetsToZeroAtIndex))
  }

  def howManyObstaclesWillIHit(reader: BufferedSource = defaultReader, charsWhichAreObstacles: Seq[Char] = Seq('#'), startingCoordinates: (Int, Int) = (0, 10), maxCoordinates: (Int, Int) = (10, 10)): Int = {
    val steps = calculateSteps(startingCoordinates)
    val (_, startingYCoordinate) = startingCoordinates
    val (maxXCoordinate, maxYCoordinate) = maxCoordinates
    reader.getLines.zipWithIndex.count {
      case (lineOfVision, index) if maxYCoordinate - index <= startingYCoordinate =>
        val currentYCoordinate = maxYCoordinate - index
        val grabCharAtIndex = steps.find {
          case (_, y) => y == currentYCoordinate
        }.map {
          case (x, _) => correctXCoordinate(x, maxXCoordinate)
        }
        val char = lineOfVision.lift(grabCharAtIndex.get).get
        charsWhichAreObstacles.contains(char)
      case (_, _ ) => false
    }
  }
}
