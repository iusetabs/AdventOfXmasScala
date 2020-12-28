package sixth

import RichClasses.SharedValues
import scala.annotation.tailrec

class CustomsForm extends SharedValues {
  val SOURCE_CONFIG_KEY: String = "sixthday.source.path"

  @tailrec
  final def countCombinedPositiveAnswers(iter: Iterator[String], positiveCount: Int = 0, isPart1: Boolean): Int = {
    if ( iter.isEmpty ) {
      positiveCount
    } else {
      val countForThisGroup = if (isPart1) {
        iter.takeWhile(_.trim.nonEmpty).toList.flatMap(_.toCharArray).toSet.size
      } else {
        val answersByPerson = iter.takeWhile(_.trim.nonEmpty).toList.map(_.toCharArray)
        val numberOfAnswersPerQuestion = answersByPerson.flatten.groupBy(i => i)
        numberOfAnswersPerQuestion.count{ case ( _, listOfChars) => listOfChars.length == answersByPerson.length }
      }
      countCombinedPositiveAnswers(iter, positiveCount + countForThisGroup, isPart1)
    }
  }

}
