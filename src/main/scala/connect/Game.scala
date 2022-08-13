package connect

import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

object Game {

  def main(args: Array[String]): Unit = {
    playNewGame()
    offerRematch()
  }

  def offerRematch(): Unit = {
    println("Play again? [y/n]")
    val shouldRestart = readLine()
    if (shouldRestart == "y") {
      playNewGame()
      offerRematch()
    } else if (shouldRestart == "n") {
      println("Thanks for playing, bye!")
    } else {
      offerRematch()
    }
  }

  def playNewGame(): Unit = {
    val grid = new Grid()
    var state = grid.checkState()

    while (!state.isOver) {
      println(grid)
      val input = readLine()
      Try(input.toInt) match {
        case Failure(exception) =>
          println(
            s"Input must be a valid column number between 0 and ${Grid.Columns}"
          )
        case Success(value) =>
          grid.play(value)
          val newState = grid.checkState()
          if (state == newState) {
            println("Illegal move")
          } else {
            state = newState
          }
      }
    }

    println(grid)
  }

}
