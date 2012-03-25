package server
import actors.Actor
import java.io._
class Saver(val filename: String) extends Actor {
  val out = new BufferedWriter(new FileWriter(new File(filename)))
  def act() = {
    import ServerInternalMessage._
    loop {
      receive {
        case SaveResult(id, result) => {
          out write id.toString + ":"
          result foreach { r =>
            out write r.toString + " "
          }
          out write "\n"
          out flush
        }
        case Quit => {
          out.close()
          exit()
        }
      }
    }
  }
}