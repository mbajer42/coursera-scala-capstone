package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait ManipulationTest extends FunSuite with Checkers {

  test("makes grid") {
    val temperatures = List[(Location, Temperature)]((Location(10, 10), 10))
    val grid = Manipulation.makeGrid(temperatures)

    val gridLocation = GridLocation(10, 10)
    val computed = grid(gridLocation)
    val expected = 10

    assert(computed == expected)
  }

  test("average grid") {
    val temperaturess = List(
      List[(Location, Temperature)]((Location(10, 10), 10)),
      List[(Location, Temperature)]((Location(10, 10), 20))
    )

    val grid = Manipulation.average(temperaturess)
    val gridLocation = GridLocation(10, 10)
    val computed = grid(gridLocation)
    val expected = 15

    assert(computed == expected)
  }

  test("deviation grid") {
    val normalTemperatures = List[(Location, Temperature)](
      (Location(10, 10), 10), (Location(20, 20), 20))
    val normalGrid = Manipulation.makeGrid(normalTemperatures)

    val otherTemperatures = List[(Location, Temperature)](
      (Location(10, 10), 15), (Location(20, 20), 20))

    val deviatedGrid = Manipulation.deviation(otherTemperatures, normalGrid)

    val expected = 5
    val computed = deviatedGrid(GridLocation(10, 10))

    assert(computed == expected)
  }
}