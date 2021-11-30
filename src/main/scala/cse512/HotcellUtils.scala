package cse512

import java.sql.Timestamp
import java.text.SimpleDateFormat
import java.util.Calendar

object HotcellUtils {
  val coordinateStep = 0.01

  def CalculateCoordinate(inputString: String, coordinateOffset: Int): Int =
  {
    // Configuration variable:
    // Coordinate step is the size of each cell on x and y
    var result = 0
    coordinateOffset match
    {
      case 0 => result = Math.floor((inputString.split(",")(0).replace("(","").toDouble/coordinateStep)).toInt
      case 1 => result = Math.floor(inputString.split(",")(1).replace(")","").toDouble/coordinateStep).toInt
      // We only consider the data from 2009 to 2012 inclusively, 4 years in total. Week 0 Day 0 is 2009-01-01
      case 2 => {
        val timestamp = HotcellUtils.timestampParser(inputString)
        result = HotcellUtils.dayOfMonth(timestamp) // Assume every month has 31 days
      }
    }
    return result
  }

  def timestampParser (timestampString: String): Timestamp =
  {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss")
    val parsedDate = dateFormat.parse(timestampString)
    val timeStamp = new Timestamp(parsedDate.getTime)
    return timeStamp
  }

  def dayOfYear (timestamp: Timestamp): Int =
  {
    val calendar = Calendar.getInstance
    calendar.setTimeInMillis(timestamp.getTime)
    return calendar.get(Calendar.DAY_OF_YEAR)
  }

  def dayOfMonth (timestamp: Timestamp): Int =
  {
    val calendar = Calendar.getInstance
    calendar.setTimeInMillis(timestamp.getTime)
    return calendar.get(Calendar.DAY_OF_MONTH)
  }

  def CountNeighbours(minX: Int, minY: Int, minZ: Int, maxX: Int, maxY: Int, maxZ: Int, inputX: Int, inputY: Int, inputZ: Int): Int =
  {
    var cell_count = 0;
    val condition_1 = 8 //if point lies in x , y and z boundary
    val condition_2 =12 //if point lies in x and y boundary
    val condition_3 =18 //if point lies in x boundary
    val condition_4 =27 //otherwise

    if (inputX == minX || inputX == maxX) {
      cell_count += 1;
    }

    if (inputY == minY || inputY == maxY) {
      cell_count += 1;
    }

    if (inputZ == minZ || inputZ == maxZ) {
      cell_count += 1;
    }

    if (cell_count == 1) {
      return condition_3;
    }
    else if (cell_count == 2)
    {
      return condition_2;
    }
    else if (cell_count == 3)
    {
      return condition_1;
    }
    else
    {
      return condition_4;
    }
  }

  def zscore(x: Int, y: Int, z: Int, mean:Double, stdDev: Double, countn: Int, sigma: Int, numcells: Int): Double =
  {
    val numerator = (sigma.toDouble - (mean*countn.toDouble))
    val denominator = stdDev*math.sqrt((((numcells.toDouble*countn.toDouble) -(countn.toDouble*countn.toDouble))/(numcells.toDouble-1.0).toDouble).toDouble).toDouble
    return (numerator/denominator).toDouble
  }
}
