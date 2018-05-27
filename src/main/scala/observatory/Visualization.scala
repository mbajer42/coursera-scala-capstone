package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  private val MeanEarthRadius = 6371 // km
  private val Width = 360 // width of picture in pixels
  private val Height = 180 // height of picutre in pixels

  /**
    *
    * @param l1 First location
    * @param l2 Second location
    * @return Great-circle distance
    */
  def distance(l1: Location, l2: Location): Double = {
    if (l1 == l2) {
      0
    } else {
      val lat1 = toRadians(l1.lat)
      val lat2 = toRadians(l2.lat)
      val deltaLongitudes = abs(toRadians(l1.lon - l2.lon))
      MeanEarthRadius * acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(deltaLongitudes))
    }
  }

  /**
    *
    * @param distance Distance between 2 locations
    * @return Simple IDW as defined by Shepard
    */
  def weightingFunction(distance: Double): Double = {
    1 / pow(distance, 6)
  }


  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {

    val distanceTemperatures = temperatures.map(entry => (distance(entry._1, location), entry._2))

    val minDistance = distanceTemperatures.minBy(_._1)

    if (minDistance._1 <= 1) {
      minDistance._2
    } else {
      val (numerator, denominator) = distanceTemperatures
        .map {
          case (distance, temperature) =>
            val weight = weightingFunction(distance)
            (weight * temperature, weight)
        }
        .reduce((entry1, entry2) => (entry1._1 + entry2._1, entry1._2 + entry2._2))
      numerator / denominator
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val equalTemperature = points.find(_._1 == value)
    equalTemperature match {
      case Some((_, color)) => color
      case _ => {

        val (colder, warmer) = points.partition(_._1 < value)

        if (colder.isEmpty) {
          warmer.minBy(_._1)._2
        } else if (warmer.isEmpty) {
          colder.maxBy(_._1)._2
        } else {
          val closestColder = colder.maxBy(_._1)
          val closestWarmer = warmer.minBy(_._1)

          val warmerDiff = closestWarmer._1 - value
          val colderDiff = value - closestColder._1
          val span = closestWarmer._1 - closestColder._1

          def interpolate(x1: Int, x2: Int): Int = ((x1 * colderDiff + x2 * warmerDiff) / span).round.toInt

          Color(
            interpolate(closestWarmer._2.red, closestColder._2.red),
            interpolate(closestWarmer._2.green, closestColder._2.green),
            interpolate(closestWarmer._2.blue, closestColder._2.blue)
          )
        }
      }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val pixels = new Array[Pixel](Width * Height)

    val coords = for {
      x <- 0 until Width
      y <- 0 until Height
    } yield (x, y)

    coords.par.foreach(coord => {
      val (x, y) = coord
      val location = xyToLocation(x, y)
      val temperature = predictTemperature(temperatures, location)
      val color = interpolateColor(colors, temperature)
      pixels(x + y * Width) = Pixel(color.red, color.green, color.blue, 255)
    })

    Image(Width, Height, pixels)
  }

  def xyToLocation(x: Int, y: Int): Location = Location(90 - y, x - 180)

}

