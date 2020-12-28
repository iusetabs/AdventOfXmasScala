package forth

object Validations {

  /** Return the appropriate validator for the given key */
  def bestValidation(key: String, value: String): Validations = {
    key match {
      case "byr" | "iyr" | "eyr" =>   YearValidations(key, value)
      case "hcl" | "ecl" =>           ColourValidations(key, value)
      case "hgt"  =>                  HeightValidations(key, value)
      case "pid" =>                   NumberValidations(key, value)
      case _ => ???
    }
  }

}

trait Validations {
  val key: String // each key has a different condition for validation
  val value: String // the value to be validated

  def isValid: Boolean // the validation function
}

/*
 byr (Birth Year) - four digits; at least 1920 and at most 2002.
 iyr (Issue Year) - four digits; at least 2010 and at most 2020.
 eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
*/
case class YearValidations(key: String, value: String) extends Validations {
  def isValid: Boolean = {
    if ( key == "byr") {
      value.toIntOption.exists(year => year >= 1920 && year <= 2002)
    } else if ( key == "iyr") {
      value.toIntOption.exists(year => year >= 2010 && year <= 2020)
    } else if ( key == "eyr") {
      value.toIntOption.exists(year => year >= 2020 && year <= 2030)
    } else {
      ???
    }
  }
}

/*
 hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
 ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
 */
case class ColourValidations(key: String, value: String) extends Validations {
  def isValid: Boolean = {
    if ( key == "hcl" ) {
      val regex = """^([0-9]|[a-f]){6}$""".r
      value.headOption.contains('#') && regex.findFirstIn(value.substring(1)).isDefined
    } else if ( key == "ecl") {
      val validValues = Seq("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
      validValues.contains(value)
    } else {
      ???
    }
  }
}

/*
 hgt (Height) - a number followed by either cm or in:
 If cm, the number must be at least 150 and at most 193.
 If in, the number must be at least 59 and at most 76.
 */
case class HeightValidations(key: String, value: String) extends Validations {
  def isValid: Boolean = {
    if ( key == "hgt") {
      val unit = value.takeRight(2)
      val maybeNumber = value.dropRight(2).toIntOption
      unit == "cm" && maybeNumber.exists(n => n <= 193 && n >= 150) || unit == "in" && maybeNumber.exists(n => n <= 76 && n >= 59)
    } else {
      ???
    }
  }
}

/*
  pid (Passport ID) - a nine-digit number, including leading zeroes.
 */
case class NumberValidations(key: String, value: String) extends Validations {
  def isValid: Boolean = {
    if ( key == "pid" ) {
      val regex = """^[0-9]{9}$""".r
      regex.findFirstIn(value).isDefined
    } else {
      ???
    }
  }
}
