package observatory


import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait VisualizationTest extends FunSuite with Checkers {

  private val colorScale = List((0.0, Color(255, 0, 0)), (1.0, Color(0, 0, 255)))
  private val temperatures = List[(Location, Temperature)]((Location(0, 0), 10), (Location(10, 10), 20))

  test("color interpolation 1") {
    val value = 0.25

    val computed = Visualization.interpolateColor(colorScale, value)
    val expected = Color(191, 0, 64)

    assert(computed == expected)
  }

  test("color interpolation 2") {
    val value = 2.0
    val computed = Visualization.interpolateColor(colorScale, value)
    val expected = Color(0, 0, 255)

    assert(computed == expected)
  }

  test("temperature prediction 1") {
    val location = Location(10, 10)
    val computed = Visualization.predictTemperature(temperatures, location)
    val expected = 20

    assert(computed == expected)
  }

  test("save image") {
    val year = 1975
    val stationFile = "/stations.csv"
    val yearFile = "/1975.csv"

    val records = Extraction.locateTemperatures(year, stationFile, yearFile)
    val averageRecords = Extraction.locationYearlyAverageRecords(records)
    val tempToCol = List[(Temperature, Color)]((60, Color(255, 255, 255)), (32, Color(255, 0, 0)),
      (12, Color(255, 255, 0)), (0, Color(0, 255, 255)), (-15, Color(0, 0, 255)), (-27, Color(255, 0, 255)),
      (-50, Color(33, 0, 107)), (-60, Color(0, 0, 0)))

    val image = Visualization.visualize(averageRecords, tempToCol)

    assert(image != null)
  }
}
