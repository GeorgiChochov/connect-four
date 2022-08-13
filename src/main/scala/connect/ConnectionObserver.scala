package connect

import connect.Grid.{StateObserver, WinLength}

import scala.collection.mutable

class ConnectionObserver(stateObserver: StateObserver) {

  private var tokens: List[Token] = Nil

  private val coords = mutable.ListBuffer[(Int, Int)]()

  def addCoord(row: Int, col: Int): Unit = {
    coords.addOne((row, col))
  }

  def notify(token: Token): Unit = {
    tokens = token :: tokens
    val isSingleColor = tokens.forall(_ == token)

//    println(
//      s"coords: ${coords.toList}: ${tokens.size}/$WinLength, ${tokens
//        .mkString(", ")}: $isSingleColor"
//    )
    if (tokens.size == WinLength && isSingleColor) {
      stateObserver.notify(token)
    }
  }

  def undo(): Unit = {
    tokens = tokens.tail
  }
}
