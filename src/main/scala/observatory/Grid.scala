package observatory

object Grid {
  val Width = 360
  val Height = 180

  /**
    *
    * @param temperatures Known temperatures
    * @return Grid temperatures at specific location
    */
  def apply(temperatures: Iterable[(Location, Temperature)]): Grid = {
    val grid = new Grid

    for {
      lat <- Range(90, -90, -1)
      lon <- -180 until 180
    } {
      val position = grid.latLonToPosition(lat, lon)
      grid.temperatures(position) = Visualization.predictTemperature(temperatures, Location(lat, lon))
    }

    grid
  }

  /**
    *
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return Grid which is the result of averaging the temperature of the years
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): Grid = {
    val grid = temperaturess
      .map(temperatures => Grid(temperatures))
      .reduce(_ + _)

    for (i <- grid.temperatures.indices) {
      grid.temperatures(i) /= temperaturess.size
    }

    grid
  }

}

class Grid private() {

  private val temperatures = new Array[Temperature](Grid.Width * Grid.Height)

  /**
    *
    * @param gridLocation Location in latitude in [-89, 90] and longitude in [-180, 179]
    * @return associated temperature at this location
    */
  def get(gridLocation: GridLocation): Temperature = {
    temperatures(latLonToPosition(gridLocation.lat, gridLocation.lon))
  }

  private def latLonToPosition(lat: Int, lon: Int): Int = {
    val x = 180 + lon
    val y = 90 - lat
    y * Grid.Width + x
  }

  private def +(that: Grid): Grid = {
    val grid = new Grid
    for (i <- temperatures.indices) {
      grid.temperatures(i) = this.temperatures(i) + that.temperatures(i)
    }
    grid
  }

}

