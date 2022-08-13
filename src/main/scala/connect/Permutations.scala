package connect

import connect.Bot.fileName
import connect.Grid.Columns

import java.io.{File, PrintWriter}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Permutations {

  def main(args: Array[String]): Unit = {
    val f = Future.sequence((0 until Columns).map { startingColumn =>
      Future {
        recursivelyTraverse(Seq(startingColumn))
      }
    })

    Await.ready(f, Duration.Inf)
  }

  private def recursivelyTraverse(startingMoves: Seq[Int] = Seq.empty): Unit = {
    val grid = new Grid()
    startingMoves.foreach(grid.play)
    val startingMovesStr = startingMoves.mkString
    val pw = new PrintWriter(new File(s"$fileName.$startingMovesStr"))

    def helper(moves: String): Unit = {
      if (grid.checkState().isOver) {
        pw.write(s"$moves,${grid.checkState()}\n")
      } else {
        (0 until Columns).toList.foreach { nextCol =>
          val wasLegalMove = grid.play(nextCol)
          if (wasLegalMove) {
            helper(s"$moves$nextCol")
            grid.undo()
          }
        }
      }
    }

    helper(startingMovesStr)
    pw.close()
  }

}
