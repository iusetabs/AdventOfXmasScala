package main.scala.forth

import RichClasses.SharedValues
import forth.Validations
import scala.annotation.tailrec

/** Return valid number of passports */
class ValidPassports extends SharedValues  {

  lazy val SOURCE_CONFIG_KEY: String = "forthday.source.path"
  lazy val PART1_COMPULSORY_FIELDS = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  /** All compulsory fields must be present as keys in the string */
  def passportIsValid(passportMetadata: String, compulsoryFields: List[String] = PART1_COMPULSORY_FIELDS, isPart1: Boolean): Boolean = {
    val metadataAsMap = passportMetadata.split(" ").map {
      keyAndValue => keyAndValue.split(":")
    }.flatMap {
        keyAndValueAsList => keyAndValueAsList.headOption.map { key => ( key, keyAndValueAsList.drop(1).mkString ) }
    }.toMap
    if (isPart1) {
      compulsoryFields.forall(metadataAsMap.keys.toList.contains)
    } else { // isPart2
      compulsoryFields.forall(field => metadataAsMap.get(field).exists{
        value =>
          val validator = Validations.bestValidation(field, value)
          validator.isValid
      })
    }
  }

  /** Negation of passport is valid. For easier reading */
  def passportIsInvalid(passportMetadata: String, compulsoryFields: List[String] = PART1_COMPULSORY_FIELDS, isPart1: Boolean): Boolean = {
    !passportIsValid(passportMetadata, compulsoryFields, isPart1)
  }

  /** Will recursively loop until the iterator is empty
   *  Will gather lines until it encounters an empty line, passes this combined string to the passport validity checker */
  @tailrec
  final def howManyPassportsAreValid(iter: Iterator[String], report: Report = Report(0, 0), isPart1: Boolean = true): Report = {
    if ( iter.isEmpty ) {
      report
    } else {
      val passportMetadata = iter.takeWhile(!_.trim.isEmpty).mkString(" ")
      val updatedReport = report.copy(
        totalPassportsScanned = report.totalPassportsScanned + 1,
        validPassports = if (passportIsValid(passportMetadata, isPart1 = isPart1)) {
          report.validPassports + 1
        } else {
          report.validPassports
        }
      )
      howManyPassportsAreValid(iter, updatedReport, isPart1)
    }
  }

  /** Simple case class to record passport scan metrics */
  case class Report(validPassports: Int, totalPassportsScanned: Int) {
    def invalidPassports: Int = totalPassportsScanned - validPassports

    override def toString: String = s"totalPassportsScanned=$totalPassportsScanned, validPassports=$validPassports, invalidPassports=$invalidPassports"
  }
}
