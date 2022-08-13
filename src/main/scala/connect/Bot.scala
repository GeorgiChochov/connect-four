package connect

import connect.GameState.{BlueWin, RedWin, Tie}
import connect.Grid.{Columns, Rows, WinLength}

import java.io.File
import scala.io.Source

object Bot {

  val start = System.currentTimeMillis()

  val fileName = s"${Rows}x${Columns}x${WinLength}.cfb"
  val delim = ','

  def main(args: Array[String]): Unit = {
    val playedGames = if (new File(fileName).exists) {
      loadFile()
    } else {
      Map.empty
    }

    val outcomes = playedGames.values.toSeq
    val tiesCount = outcomes.count(_ == Tie)
    val redWinCount = outcomes.count(_ == RedWin)
    val blueWinCount = outcomes.count(_ == BlueWin)
    println(
      s"ties: $tiesCount/${playedGames.size} ${tiesCount.toDouble / playedGames.size}%"
    )
    println(
      s"redWin: $redWinCount/${playedGames.size} ${redWinCount.toDouble / playedGames.size}%"
    )
    println(
      s"blueWin: $blueWinCount/${playedGames.size} ${blueWinCount.toDouble / playedGames.size}%"
    )
    println(s"Total time: ${(System.currentTimeMillis - start) / 1000}s")
  }

  private def loadFile(): Map[String, GameState.EndState] = {
    println(s"Loading $fileName")
    val source = Source.fromFile(fileName)
    val loadedGames = source.getLines().map(parseGame)

    val result = loadedGames.toMap
    source.close()
    result
  }

  private def parseGame(game: String) = {
    val Array(moves, outcomeStr) = game.split(delim)
    val outcome = outcomeStr match {
      case "RedWin"  => RedWin
      case "BlueWin" => BlueWin
      case _         => Tie
    }
    moves -> outcome
  }

}
