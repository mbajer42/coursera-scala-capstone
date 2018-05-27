package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

trait InteractionTest extends FunSuite with Checkers {

  test("Generates image out of one tile") {
    val temperatures = List[(Location, Temperature)]((Location(10, 10), 10))
    val colors = Interaction2.temperatureColors

    val tile = Tile(0, 0, 0)

    val image = Interaction.tile(temperatures, colors, tile)

    assert(image != null)
  }

}
