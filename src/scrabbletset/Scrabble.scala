

package scrabbletset

import scala.util.Try
import scala.util.Success
import scala.util.Failure

object Scrabble extends App {

  case class Tile(name: Char, count: Int, value: Int) {

    def -(c: Int): Tile = {
      if (c <= count) {
        Tile(name, count - c, value)
      } else {
        throw new IllegalArgumentException(s"Invalid input. More ${name}'s have been taken from the bag than possible. ")
      }
    }

  }

  case class ScrabbleSet(tiles: List[Tile]) {

    def -(input: String): Try[ScrabbleSet] = {

      // Map input character to their number of occurrences
      val charByCount = input.toCharArray().groupBy(c => c).map {
        case (c, l) => (c, l.length)
      }

      // Create a new scrabblset with updated tiles 
      Try({
        ScrabbleSet(tiles.map(t => {
          t - charByCount.getOrElse(t.name, 0)
        }))
      })

    }

    def +(tile: Tile): ScrabbleSet = {
      ScrabbleSet(tile :: tiles)
    }

    override def toString: String = {
      val tilesByCount = tiles.groupBy(_.count).toList.sortBy(_._1)(Ordering[Int].reverse)
      tilesByCount.foldLeft("") {
        case (acc, (count, tiles)) => {
          val sortedTileNames = tiles.sortBy(_.name).map(t => t.name).mkString(",")
          acc + count + ": " + sortedTileNames + "\n"
        }
      }
    }

  }

  // Load scrabble set from definition file
  import ScrabbletSetLoader.loadScrabbletSet

  def play(inputs: List[String]) = {

    var scrabbleSet = loadScrabbletSet
    println("Initial scrabble set")
    println(scrabbleSet)

    inputs.foreach(input => {
      println("About to play on initial set: " + input)
      scrabbleSet - input match {
        case Success(newScrabbleSet) => {
          println(newScrabbleSet)
        }
        case Failure(f) => println(f.getMessage)
      }
    })
  }

  // Play a list of sequence
  play(List("PQAREIOURSTHGWIOAE_", "LQTOONOEFFJZT", "AXHDRUIOR_XHJZUQEE"))

}