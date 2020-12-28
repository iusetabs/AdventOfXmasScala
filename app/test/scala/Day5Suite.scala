package scala

import RichClasses.SharedValues
import fifth.FindMySeat
import org.scalatest.FunSuite

/** Tests around Day5 tasks */
class Day5Suite extends FunSuite with SharedValues {

  val testInstruction = "FBFBBFFRLR"
  val SOURCE_CONFIG_KEY: String = "fifthday.example.source.path"
  val findMySeat = new FindMySeat
  case class ExpectedOutput(row: Int, column: Int, seatId: Int)

  test("row instructions should be calculated correct") {
    val directionsToRow = FindMySeat.getRowInstructions(testInstruction)
    assert(findMySeat.findMyRowOrSeat(directionsToRow, FindMySeat.MIN_ROW_ID, FindMySeat.MAX_ROW_ID, findRow = true) == 44)
  }

  test("seat instructions should be calculated correct") {
    val directionsToSeat = FindMySeat.getSeatInstructions(testInstruction)
    assert(findMySeat.findMyRowOrSeat(directionsToSeat, FindMySeat.MIN_SEAT_ID, FindMySeat.MAX_SEAT_ID, findRow = false) == 5)
  }

  test("PART1: Given example input correct row, seat and ID should be returned") {
    val expected = Seq((70,7,567), (14,7,119), (102,4,820)).map{case (row, col, id) => ExpectedOutput(row, col, id)}
    defaultReader.getLines.zipWithIndex.foreach {
      case (instruction, index ) =>
        val (directionsToRow, directionsToSeat) = (FindMySeat.getRowInstructions(instruction), FindMySeat.getSeatInstructions(instruction))
        val row = findMySeat.findMyRowOrSeat(directionsToRow, FindMySeat.MIN_ROW_ID, FindMySeat.MAX_ROW_ID, findRow = true)
        val seat = findMySeat.findMyRowOrSeat(directionsToSeat, FindMySeat.MIN_SEAT_ID, FindMySeat.MAX_SEAT_ID, findRow = false)
        val id = findMySeat.calculateSeatId(row, seat)
        val expectedOutput = expected(index)
        assert(row == expectedOutput.row)
        assert(seat == expectedOutput.column)
        assert(id == expectedOutput.seatId)
    }
  }

  test("PART1: Given example input the correct max should be returned") {
    assert(findMySeat.getHighestSeatId(defaultReader.getLines) == 820)
  }

  test("PART1: Given test input the correct max should be returned") {
    val reader = getReader(getStringFromConfig(findMySeat.SOURCE_CONFIG_KEY)).getLines
    println(findMySeat.getHighestSeatId(reader))
  }

  test("PART2: Print the answer") {
    val reader = getReader(getStringFromConfig(findMySeat.SOURCE_CONFIG_KEY)).getLines
    println(findMySeat.findMySeat(reader, Seq.empty))
  }

}
