import first.Accounting2020
import org.scalatest.FunSuite

/** Tests around Accounting 2020 */
class Day1Suite extends FunSuite {


  test("all elements in the array (1,2,3) should multiply to 6") {
    val testSeq: List[Long] = List(1,2,3)
    assert(Accounting2020.multipleByEach(testSeq) == 6)
  }

  test("all elements in the array ( 1, 2..., 9, 10) should multiple to 3628800") {
    val testSeq = (0 until 10).toList.map(_+1.toLong)
    assert(Accounting2020.multipleByEach(testSeq) == 3628800, s"$testSeq elements did not multiply to 3628800!")
  }

  test("Part1") {
    assert(Accounting2020.execute(howManyNumbersToAdd = 2).toSet == List(337, 1683).toSet)
  }

  test("Part2") {
    Accounting2020.execute(howManyNumbersToAdd = 3)
  }

}
