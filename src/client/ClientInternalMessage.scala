package client

abstract class ClientInternalMessage
object ClientInternalMessage {
  case class AssignJob[A](id: Any, job: Unit => A) extends ClientInternalMessage
  case class JobResult[A](id: Any, result: A) extends ClientInternalMessage
  case object Quit extends ClientInternalMessage
}