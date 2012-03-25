package client
import actors.Actor
class Worker extends Actor {
  private var _onjob = false
  def onJob = _onjob
  override def act() = {
    import ClientInternalMessage._
    println("Worker start")
    loop {
      receive {
        case AssignJob(id, job) => {
          _onjob = true
          println("Worker job assigned " + id)
          sender ! JobResult(id, job.apply())
          println("Worker job done " + id)
          _onjob = false
        }
        case Quit => exit()
      }
    }
  }
}