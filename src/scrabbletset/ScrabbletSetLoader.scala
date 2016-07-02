package scrabbletset

object ScrabbletSetLoader {

  import Scrabble._
  
  def loadScrabbletSet: ScrabbleSet = {
    var result = ScrabbleSet(List.empty)
    val bufferedSource = io.Source.fromFile("/Users/ccordenier/workspace/ScrabbleSet/src/scrabbletset/scrabbleset_definition.csv")
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      result = result + Tile(cols(0).charAt(0), Integer.valueOf(cols(1)), Integer.valueOf(cols(2)))
    }
    bufferedSource.close
    result
  }

}