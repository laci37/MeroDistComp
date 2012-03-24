package server
import actors._
abstract class ServerInternalMessage
object ServerInternalMessage {
  case class SendClassData(name: String, to: OutputChannel[Any]) extends ServerInternalMessage
  case class SaveResult[A](id:Any,result:Array[A]) extends ServerInternalMessage
  case object Quit extends ServerInternalMessage
  case object CyclicCheck extends ServerInternalMessage
}