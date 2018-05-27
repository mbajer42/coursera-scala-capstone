package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math.{Pi, atan, pow, sinh}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  val Width = 256
  val Height = 256
  val SubtileZoom = 8

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    Location(latitude(tile), longitude(tile))
  }

  /**
    *
    * @param tile Tile coordinates
    * @return The longitude of the top-left corner of the tile
    */
  def longitude(tile: Tile): Double = {
    tile.x / pow(2, tile.zoom) * 360 - 180
  }

  /**
    *
    * @param tile Tile coordinates
    * @return The latitude of the top-left corner of the tile
    */
  def latitude(tile: Tile): Double = {
    atan(sinh(Pi - tile.y / pow(2, tile.zoom) * 2 * Pi)) * 180 / Pi
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val pixels = new Array[Pixel](Width * Height)
    val subtileScale = 1 << SubtileZoom // (2 ^ SubtileZoom)
    val x0 = tile.x * subtileScale
    val y0 = tile.y * subtileScale
    val zoom = tile.zoom

    val coords = for {
      x <- 0 until Width
      y <- 0 until Height
    } yield (x, y)

    coords.par.foreach(coord => {
      val (x, y) = coord
      val location = tileLocation(Tile(x + x0, y + y0, zoom + SubtileZoom))
      val temperature = Visualization.predictTemperature(temperatures, location)
      val color = Visualization.interpolateColor(colors, temperature)
      pixels(x + y * Width) = Pixel(color.red, color.green, color.blue, 127)
    })

    Image(Width, Height, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Year, Data)],
                           generateImage: (Year, Tile, Data) => Unit
                         ): Unit = {
    for {
      (year, data) <- yearlyData
      zoom <- 0 until 4
      x <- 0 until pow(2, zoom).toInt
      y <- 0 until pow(2, zoom).toInt
    } generateImage(year, Tile(x, y, zoom), data)

  }

}
