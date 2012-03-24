package client

import actors._
import remote0._

class ClassRequester(serv: Node, sym: Symbol, classname: String) extends Actor {
  var data: Option[Array[Byte]] = None
  var err = false
  def act() = {
    RemoteActor.alive(TcpService.generatePort)
    val server = RemoteActor.select(serv, sym)
    import msg.NetworkMessage._
    if (ClientEntry.verbose) println("requesting remote class " + classname + " from " + server)
    server ! ClassRequest(classname)
    if (ClientEntry.verbose) println("ClassReq(" + classname + ") start")
    loop {
      receiveWithin(ClientEntry.classReqTimeOut) {
        case ClassInfo(classname, bytes) => {
          if (ClientEntry.verbose) println("ClassReq(" + classname + ") got reply")
          data = Some(bytes)
          exit()
        }
        case ClassNotFound => {
          println("ClassReq(" + classname + ") got no data")
          err = true
          exit()
        }
        case _ =>
      }
      println("classreq timeout trying to send buffer again")
      RemoteActor.selfKernel.service.asInstanceOf[TcpService].sendBuffered(serv)
    }
  }
}