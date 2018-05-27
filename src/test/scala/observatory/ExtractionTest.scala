package observatory

import org.scalatest.FunSuite

trait ExtractionTest extends FunSuite {


  test("2015: locating temperatures") {
    val year = 2015
    val stationFile = "/stations.csv"
    val yearFile = "/2015.csv"

    val temperatures = Extraction.locateTemperatures(year, stationFile, yearFile)

    assert(temperatures.nonEmpty)
  }

  test("2015: averaging temperatures") {
    val year = 2015
    val stationFile = "/stations.csv"
    val yearFile = "/2015.csv"

    val locatedTemperatures = Extraction.locateTemperatures(year, stationFile, yearFile)
    val averagedTemperatures = Extraction.locationYearlyAverageRecords(locatedTemperatures)

    assert(averagedTemperatures.nonEmpty)
  }

}