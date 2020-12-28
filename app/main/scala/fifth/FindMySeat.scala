package fifth

import RichClasses.SharedValues
import fifth.FindMySeat.{FRONT_ROWS, LEFT_SEATS}
import scala.annotation.tailrec

class FindMySeat extends SharedValues {
  val SOURCE_CONFIG_KEY: String = "fifthday.source.path"

  @tailrec
  final def findMyRowOrSeat(directions: String, minId: Int, maxId: Int, findRow: Boolean): Int = {
    val upperId = if ( findRow ) FRONT_ROWS else LEFT_SEATS
    if ( minId == maxId ) {
      minId
    } else {
      val nextDirection = directions.head
      val median = (maxId - minId) / 2
      val (newMin, newMax) = if ( nextDirection == upperId ) {
        (minId, minId + median)
      } else {
        (minId + median+1, maxId)
      }
      findMyRowOrSeat(directions.drop(1), newMin, newMax, findRow)
    }
  }

  /** multiply the row by 8, then add the column */
  def calculateSeatId(row: Int, seat: Int): Int = {
    (row * 8) + seat
  }

  @tailrec
  final def getHighestSeatId(iter: Iterator[String], maxSeatId: Int = 0): Int = {
    if ( iter.isEmpty ) {
      maxSeatId
    } else {
      val instruction = iter.next
      val (directionsToRow, directionsToSeat) = (FindMySeat.getRowInstructions(instruction), FindMySeat.getSeatInstructions(instruction))
      val row = findMyRowOrSeat(directionsToRow, FindMySeat.MIN_ROW_ID, FindMySeat.MAX_ROW_ID, findRow = true)
      val seat = findMyRowOrSeat(directionsToSeat, FindMySeat.MIN_SEAT_ID, FindMySeat.MAX_SEAT_ID, findRow = false)
      val id = calculateSeatId(row, seat)
      getHighestSeatId(iter, Math.max(maxSeatId, id))
    }
  }

  @tailrec
  final def findMySeat(iter: Iterator[String], filledSeats: Seq[(Int, Int)]): Int = {

    @tailrec
    def findSeatsWithNonFreeAdjacentSeats(freeSeatIds: List[Int], possibleSeats: List[Int]): List[Int] = {
      if (freeSeatIds.isEmpty) {
        possibleSeats
      } else {
        val updatedPossibleSeats = (for {
          previousFreeSeatId <- freeSeatIds.headOption
          nextFreeSeatId <- freeSeatIds.lift(2)
          currentSeatId <- freeSeatIds.lift(1) if currentSeatId - previousFreeSeatId > 1 && nextFreeSeatId - currentSeatId > 1
        } yield {
          possibleSeats :+ currentSeatId
        }).getOrElse(possibleSeats)

        findSeatsWithNonFreeAdjacentSeats(freeSeatIds.drop(1), updatedPossibleSeats)
      }
    }

    if ( iter.isEmpty ) {
      val freeSeatsByRow = FindMySeat.ALL_SEATS_BY_ROW.diff(filledSeats.toSet).toList.sorted
      val freeSeatIds = freeSeatsByRow.map{ case (row, seat) => calculateSeatId(row, seat)}
      val possibleSeats = findSeatsWithNonFreeAdjacentSeats(freeSeatIds, List.empty)
      assert(possibleSeats.length == 1)
      possibleSeats.head
    } else {
      val instruction = iter.next
      val (directionsToRow, directionsToSeat) = (FindMySeat.getRowInstructions(instruction), FindMySeat.getSeatInstructions(instruction))
      val row = findMyRowOrSeat(directionsToRow, FindMySeat.MIN_ROW_ID, FindMySeat.MAX_ROW_ID, findRow = true)
      val seat = findMyRowOrSeat(directionsToSeat, FindMySeat.MIN_SEAT_ID, FindMySeat.MAX_SEAT_ID, findRow = false)
      findMySeat(iter, filledSeats :+ (row, seat))
    }
  }

}

/** Primarily for constants */
object FindMySeat {
  val MIN_ROW_ID = 0
  val MAX_ROW_ID = 127
  val MIN_SEAT_ID = 0
  val MAX_SEAT_ID = 7

  val FRONT_ROWS = 'F'
  val BACK_ROWS = 'B'
  val LEFT_SEATS = 'L'
  val RIGHT_SEATS = 'R'

  lazy val ALL_SEATS_BY_ROW: Set[(Int, Int)] = (MIN_ROW_ID to MAX_ROW_ID).toSet.flatMap{row: Int => (MIN_SEAT_ID to MAX_SEAT_ID).toSet.map{seat: Int => (row, seat)}}

  def getRowInstructions(instructions: String): String = {
    instructions.dropRight(3)
  }

  def getSeatInstructions(instructions: String): String = {
    instructions.takeRight(3)
  }
}
