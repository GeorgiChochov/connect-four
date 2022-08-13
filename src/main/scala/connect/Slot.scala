package connect

import scala.collection.mutable

class Slot() {

  private var token: Option[Token] = None

  private val observers = mutable.ListBuffer[ConnectionObserver]()

  def registerObserver(observer: ConnectionObserver): Unit = {
    observers += observer
  }

  def setToken(token: Token): Unit = {
    this.token = Some(token)
    observers.foreach(_.notify(token))
  }

  def undo(): Unit = {
    this.token = None
    observers.foreach(_.undo())
  }

  def getToken: Option[Token] = token

  def isEmpty: Boolean = token.isEmpty
}
