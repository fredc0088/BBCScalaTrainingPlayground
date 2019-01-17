package Lab02a

import scala.util.{Failure, Success, Try}

/**
  * 1. Define a type to represent a date, consisting of day, month and year. Define a second type to represent a time as hours, minutes and seconds.
  *   Using regular expressions, develop implicit conversions from the String type to your date type, and to your time type.
  * 2. For your “Unit” types from the previous exercises, use implicit conversions and extension methods as shown in the slides to allow you to represent instances of the types using a more convenient notation, for example
  * 3.metre ( or even 3 metre )
  *
  * Verify that you can use these in constructing expressions using the values.
  */
object Main extends App {

  case class Date(day: Int, month: Int, year: Int){
    override def toString: String = s"${if(day <= 9) s"0$day" else day}/${if(month <= 9) s"0$month" else month}/$year"
    def prettyDate = s"Date is: $toString"
  }

  case class Time(hour: Int, minute: Int, second: Int){
    override def toString: String = s"${if(hour < 10) s"0$hour" else hour}:${if(minute < 10) s"0$minute" else minute}:${if(second < 10) s"0$second" else second}"
    def prettyTime = s"Time is: $toString"
  }


  object Implicits {

    implicit def toInt(string: String): Int = Try{augmentString(string).toInt}.getOrElse(0)

    implicit def toDate(date: String) = {
      val regex = "([0-9]{2})-([0-9]{2})-([0-9]{4})".r
      Try{
        val regex(day, month, year) = date
        (day,month,year)
      } match {
        case Success((day, month, year)) => Date(day, month,year)
        case Failure(_) => Date(1,1, 1000)
      }
    }

    implicit def toTime(time: String): Time = {
      val regex = "(2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])".r
      Try{
        val regex(hour, minute, second) = time
        (hour,minute,second)
      } match {
        case Success((hour, minute, second)) => Time(hour,minute,second)
        case Failure(_) => Time(0,0, 0)
      }
    }
  }

  import Implicits._

  println("01-02-1988".prettyDate)
  println("01:02:01".prettyTime)
  println("01:02-1988".prettyDate)
}
