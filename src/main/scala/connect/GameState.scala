package connect

import connect.Token.{Blue, Red}

sealed trait GameState {
  def isOver: Boolean
}

object GameState {

  sealed trait EndState extends GameState {
    val isOver: Boolean = true
  }

  case object Tie extends EndState
  case object BlueWin extends EndState
  case object RedWin extends EndState

  sealed trait ContinueState extends GameState {
    val isOver: Boolean = false
    def nextTurn: ContinueState
    def token: Token
  }

  case object BlueTurn extends ContinueState {
    val nextTurn: ContinueState = RedTurn
    val token: Token = Blue
  }
  case object RedTurn extends ContinueState {
    val nextTurn: ContinueState = BlueTurn
    val token: Token = Red
  }
}
