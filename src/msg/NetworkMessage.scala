package msg

abstract class NetworkMessage
object NetworkMessage {
  case object Hi extends NetworkMessage
  case class AssignJob[A](id: Any, job: Iterator[(Unit=>A)]) extends NetworkMessage
  case class JobAccepted(id: Any) extends NetworkMessage
  case class JobRefused(id:Any) extends NetworkMessage
  case class JobResult[A](id: Any, result: Array[A]) extends NetworkMessage
  case class ClassRequest(name: String) extends NetworkMessage
  case class ClassInfo(name:String,dta:Array[Byte]) extends NetworkMessage
  case object ClassNotFound extends NetworkMessage
  case object ExitJob extends NetworkMessage
  case object AskJob extends NetworkMessage
  case class OnJob(id: Any) extends NetworkMessage
  case object NoJob extends NetworkMessage
  case object Quit extends NetworkMessage
}