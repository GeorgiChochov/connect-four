package connect

import connect.GameState._
import connect.Grid.StateObserver
import connect.Token._

import scala.collection.mutable

object Grid {
  final val Rows = 3
  final val Columns = 3
  final val WinLength = 3

  class StateObserver(grid: Grid) {
    def notify(token: Token): Unit = {
      grid.gameState = token match {
        case Red  => RedWin
        case Blue => BlueWin
      }
    }
  }
}

class Grid() {
  import Grid.{Columns, Rows, WinLength}

  private val matrix =
    Array.tabulate[Slot](Rows, Columns)((_, _) => new Slot())

  private var gameState: GameState = RedTurn
  private val moves = new mutable.Stack[(Slot, ContinueState)]()

  val indexes = {
    val stateObserver = new StateObserver(this)
    val rowObservers = (0 until Rows).flatMap { row =>
      (0 to Columns - WinLength).map { column =>
        val rowObserver = new ConnectionObserver(stateObserver)
        (0 until WinLength).foreach { i =>
          val rowStep = row
          val columnStep = column + i
          matrix(rowStep)(columnStep).registerObserver(rowObserver)
          rowObserver.addCoord(rowStep, columnStep)
        }

        rowObserver
      }
    }
    val columnObservers = (0 to Rows - WinLength).flatMap { row =>
      (0 until Columns).map { column =>
        val columnObserver = new ConnectionObserver(stateObserver)
        (0 until WinLength).foreach { i =>
          val rowStep = row + i
          val columnStep = column
          matrix(rowStep)(columnStep).registerObserver(columnObserver)
          columnObserver.addCoord(rowStep, columnStep)
        }

        columnObserver
      }
    }
    val upDiagonalObservers = (0 to Rows - WinLength).flatMap { row =>
      (0 to Columns - WinLength).map { column =>
        val upRightDiagonalObserver = new ConnectionObserver(stateObserver)
        (0 until WinLength).foreach { i =>
          val rowStep = row + i
          val columnStep = column + i
          matrix(rowStep)(columnStep).registerObserver(upRightDiagonalObserver)
          upRightDiagonalObserver.addCoord(rowStep, columnStep)
        }

        upRightDiagonalObserver
      }
    }
    val downDiagonalObservers = (WinLength - 1 until Rows).flatMap { row =>
      (0 to Columns - WinLength).map { column =>
        val downRightDiagonalObserver = new ConnectionObserver(stateObserver)
        (0 until WinLength).foreach { i =>
          val rowStep = row - i
          val columnStep = column + i
          matrix(rowStep)(columnStep)
            .registerObserver(downRightDiagonalObserver)
          downRightDiagonalObserver.addCoord(rowStep, columnStep)
        }

        downRightDiagonalObserver
      }
    }
    rowObservers ++ columnObservers ++ upDiagonalObservers ++ downDiagonalObservers
  }

  def checkState() = {
    gameState match {
      case state: EndState => state
      case _: ContinueState =>
        if (moves.size < Rows * Columns) {
          gameState
        } else {
          Tie
        }
    }
  }

//  def checkState(): GameState = {
//    val rowState = checkRows()
//    val columnState = checkColumns()
//    val diagonalState = gameState
//    if (rowState.isOver) {
//      rowState
//    } else if (columnState.isOver) {
//      columnState
//    } else if (diagonalState.isOver) {
//      diagonalState
//    } else if (turnNumber < Rows * Columns) {
//      gameState
//    } else {
//      Tie
//    }
//  }
//
//  private def checkRows(): GameState = {
//    var currentColor: Token = Empty
//    var currentRun = 0
//    (0 until Rows).foreach { row =>
//      (0 until Columns).foreach { column =>
//        val currentSlot = matrix(row)(column)
//        if (currentSlot == Empty) {
//          currentColor = Empty
//          currentRun = 0
//        } else if (currentColor == currentSlot) {
//          currentRun += 1
//        } else {
//          currentColor = currentSlot
//          currentRun = 1
//        }
//
//        if (currentRun == WinLength) {
//          return currentSlot match {
//            case Empty => gameState
//            case Blue  => BlueWin
//            case Red   => RedWin
//          }
//        }
//      }
//    }
//    gameState
//  }
//
//  private def checkColumns(): GameState = {
//    var currentColor: Token = Empty
//    var currentRun = 0
//    var column = 0
//    var row = 0
//    while (currentRun < WinLength && column < Columns) {
//      while (currentRun < WinLength && row < Rows) {
//        val currentSlot = matrix(row)(column)
//        if (currentSlot == Empty) {
//          currentColor = Empty
//          currentRun = 0
//          row = Rows
//        } else if (currentColor == currentSlot) {
//          currentRun += 1
//        } else {
//          currentColor = currentSlot
//          currentRun = 1
//        }
//        row += 1
//      }
//
//      column += 1
//      row = 0
//    }
//
//    if (currentRun == WinLength) {
//      currentColor match {
//        case Empty => gameState
//        case Blue  => BlueWin
//        case Red   => RedWin
//      }
//    } else {
//      gameState
//    }
//  }

  def play(column: Int): Boolean = {
    if (column < 0 || column >= Columns) {
      false
    } else {
      val row = matrix.indexWhere(row => row(column).isEmpty)
      place(column, row)
    }
  }

  def undo(): Unit = {
    val (slot, state) = moves.pop()
    slot.undo()
    gameState = state
  }

  private def place(column: Int, row: Int): Boolean = {
    if (row >= 0 && row < Rows) {
      gameState match {
        case state: ContinueState =>
          val slot = matrix(row)(column)
          slot.setToken(state.token)
          moves.push((slot, state))
          if (!gameState.isOver) {
            gameState = state.nextTurn
          }
          true
        case _ =>
          false
      }
    } else {
      false
    }
  }

  override def toString: String = {
    val columnsRow =
      (0 until Columns).mkString(
        "\n  Col ",
        " ",
        s"\n${checkState()}"
      )
    matrix.zipWithIndex.reverse
      .map { case (row, index) =>
        row
          .map { slot =>
            slot.getToken match {
              case None       => " "
              case Some(Red)  => "X"
              case Some(Blue) => "O"
            }
          }
          .mkString(s"Row $index|", "|", "|")
      }
      .mkString("", "\n", columnsRow)

  }
}
