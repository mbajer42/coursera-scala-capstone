package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  val Width = 256
  val Height = 256
  val SubtileZoom = 8

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
                             point: CellPoint,
                             d00: Temperature,
                             d01: Temperature,
                             d10: Temperature,
                             d11: Temperature
                           ): Temperature = {
    val x = point.x
    val y = point.y
    d00 * (1 - x) * (1 - y) + d10 * x * (1 - y) + d01 * (1 - x) * y + d11 * x * y
  }

  def interpolate(grid: GridLocation => Temperature, location: Location): Temperature = {
    val latitude = location.lat.floor.toInt
    val longitude = location.lon.floor.toInt

    val d00 = grid(GridLocation(latitude, longitude))
    val d01 = grid(GridLocation(latitude + 1, longitude))
    val d10 = grid(GridLocation(latitude, longitude + 1))
    val d11 = grid(GridLocation(latitude + 1, longitude + 1))

    val xDelta = location.lon - longitude
    val yDelta = location.lat - latitude

    val point = CellPoint(xDelta, yDelta)

    bilinearInterpolation(point, d00, d01, d10, d11)
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
                     grid: GridLocation => Temperature,
                     colors: Iterable[(Temperature, Color)],
                     tile: Tile
                   ): Image = {
    val pixels = new Array[Pixel](Width * Height)
    val subtileScale = 1 << SubtileZoom // 2 ^ SubtileZoom
    val x0 = tile.x * subtileScale
    val y0 = tile.y * subtileScale
    val zoom = tile.zoom

    for {
      x <- 0 until Width
      y <- 0 until Height
    } {
      val tile = Tile(x + x0, y + y0, zoom + SubtileZoom)
      val location = Interaction.tileLocation(tile)
      val temperature = interpolate(grid, location)
      val color = Visualization.interpolateColor(colors, temperature)

      pixels(x + y * Width) = Pixel(color.red, color.green, color.blue, 127)
    }

    Image(Width, Height, pixels)
  }

}
