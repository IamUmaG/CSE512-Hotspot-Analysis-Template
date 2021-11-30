package cse512

object HotzoneUtils {

  def ST_Contains(queryRectangle: String, pointString: String ): Boolean = {
    val rectCoOrdinates = queryRectangle.split(",")
    val ptsCoOrdinates = pointString.split(",")

    var rectX1:Double = rectCoOrdinates(0).toDouble
    var rectY1:Double = rectCoOrdinates(1).toDouble

    var rectX2:Double = rectCoOrdinates(2).toDouble
    var rectY2:Double = rectCoOrdinates(3).toDouble

    var ptX:Double = ptsCoOrdinates(0).toDouble
    var ptY:Double = ptsCoOrdinates(1).toDouble

    var minX = math.min(rectX1,rectX2)
    var maxX = math.max(rectX1,rectX2)
    var minY = math.min(rectY1,rectY2)
    var maxY = math.max(rectY1,rectY2)

    if (ptX <= maxX && ptX >= minX && ptY <= maxY && ptY >= minY)
        true
    else
      false
  }
}
