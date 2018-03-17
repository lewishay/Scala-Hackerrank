/* Example of calling an API and putting the response into a List:
  val url = "http://api.openweathermap.org/data/2.5/forecast?mode=xml&lat=55&lon=0"
  val l: List[String] = io.Source.fromURL(url).getLines.toList
*/

// Static example response to remove API dependency for this exercise
val weather: List[String] = List(
  """<?xml version="1.0" encoding="UTF-8"?>""",
  """<current>""",
  """<city id="3345439" name="Telford"></city>""",
  """<country>GB</country>""",
  """<temperature value="273.5" min="273.15" max="274.15" unit="kelvin"></temperature>""",
  """<humidity value="69" unit="%"></humidity>""",
  """<pressure value="1010" unit="hPa"></pressure>""",
  """</current>"""
)

val cityIds: Map[String, String] = {
  val city: Option[String] = weather.find(_.contains("<city"))
  val idRegex = """[0-9]+""".r
  val nameRegex = """\b[A-Z].*?\b""".r
  city match {
    case Some(x) => Map((
      idRegex.findAllIn(x).mkString,
      nameRegex.findAllIn(x).mkString
    ))
    case None => Map(("-1", "City data not found"))
  }
}

val temperature: String = {
  val temperature: Option[String] = weather.find(_.contains("<temperature"))
  val minRegex = """min=\S*""".r
  val maxRegex = """max=\S*""".r
  val quotesExtractor = """[0-9]+.[0-9]+""".r
  temperature match {
    case Some(x) =>
      val minTemp = minRegex.findAllIn(x).mkString
      val maxTemp = maxRegex.findAllIn(x).mkString
      s"Min temperature: ${quotesExtractor.findAllIn(minTemp).mkString}, " +
        s"Max temperature: ${quotesExtractor.findAllIn(maxTemp).mkString}"
    case None => "Temperature data not found"
  }
}