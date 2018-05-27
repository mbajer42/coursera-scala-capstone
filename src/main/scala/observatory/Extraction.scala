package observatory

import java.time.LocalDate

import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction {

  private val resourceLookup = (fileName: String) => getClass.getResourceAsStream(fileName)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(
                          year: Year,
                          stationsFile: String,
                          temperaturesFile: String)
  : Iterable[(LocalDate, Location, Temperature)] = {

    val stations = getStations(stationsFile)
    val temperatures = getTemperatures(year, temperaturesFile)

    locateTemperatures(stations, temperatures)
  }

  def locateTemperatures(
                          stations: Map[Station, Location],
                          temperatures: Iterable[(Station, (LocalDate, Temperature))])
  : Iterable[(LocalDate, Location, Temperature)] = {

    temperatures
      .filter(line => stations.contains(line._1))
      .map(line => {
        val (station, (date, temperature)) = line
        val location = stations(station)
        (date, location, temperature)
      })
  }

  /**
    * @param stationsFile Path of the stations resource file to use (e.g. "/stations.csv")
    * @return A sequence containing the station and its location
    */

  def getStations(stationsFile: String): Map[Station, Location] = {
    Source
      .fromInputStream(resourceLookup(stationsFile))
      .getLines
      .map(_.split(","))
      .filter(_.length == 4)
      .map(cols => {
        val stn = cols(0)
        val wban = cols(1)
        val lat = cols(2).toDouble
        val lon = cols(3).toDouble
        Station(stn, wban) -> Location(lat, lon)
      })
      .toMap
  }

  /**
    *
    * @param year             Year number
    * @param temperaturesFile Path of the temperatures resource file to use
    * @return A sequence containing the station and the temperature recorded at a specific date
    */
  def getTemperatures(year: Year, temperaturesFile: String): Iterable[(Station, (LocalDate, Temperature))] = {
    Source
      .fromInputStream(resourceLookup(temperaturesFile))
      .getLines
      .toList.view
      .map(_.split(","))
      .filter(_.length == 5)
      .map(cols => {
        val stn = cols(0)
        val wban = cols(1)
        val month = cols(2).toInt
        val day = cols(3).toInt
        val temperature = fahrenheitToCelsius(cols(4).toDouble)
        (Station(stn, wban), (LocalDate.of(year, month, day), temperature))
      })
  }

  private def fahrenheitToCelsius(fahrenheit: Temperature): Temperature = (fahrenheit - 32) / 1.8

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(
                                    records: Iterable[(LocalDate, Location, Temperature)])
  : Iterable[(Location, Temperature)] = {

    records
      .map(entry => (entry._2, entry._3))
      .groupBy(_._1)
      .map(pair => {
        val avgTemperature = avg(pair._2.map(_._2))
        (pair._1, avgTemperature)
      })
  }

  def avg(temperatures: Iterable[Temperature]): Temperature = temperatures.sum / temperatures.size

}
