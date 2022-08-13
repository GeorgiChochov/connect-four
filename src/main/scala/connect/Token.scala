package connect

sealed trait Token

object Token {
  case object Red extends Token
  case object Blue extends Token
}
