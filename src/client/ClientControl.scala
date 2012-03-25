package client

import actors._
import scala.actors.remote0._
import RemoteActor._
import msg._
import collection.mutable.ArrayBuffer

class ClientControl(val port: Int, val name: Symbol, hiserv: OutputChannel[Any]) extends Actor {
  var curJob: Option[Iterator[(Unit => Any)]] = None
  var curJobId: Option[Any] = None
  var server: Option[OutputChannel[Any]] = None
  var resultBuffer = new ArrayBuffer[Any]
  var workers: List[Worker] = Nil
  var jobsWorking: List[Int] = Nil
  var nextId = 0

  override def act() = {
    println("Start\007\b")
    println("ClassLoader: " + RemoteActor.classLoader.toString())
    alive(port)
    register(name, this)
    hiserv ! NetworkMessage.Hi
    loop {
      receive {
        case msg: NetworkMessage => handleNetwork(msg)
        case msg: ClientInternalMessage => handleInternal(msg)
        case _ => println("wrong msg for ClientControl")
      }
    }
  }

  def handleNetwork(msg: NetworkMessage): Unit = {
    import NetworkMessage._
    if (ClientEntry.verbose) println(msg)
    msg match {

      case AssignJob(id, newjob) => {
        if (curJob.isDefined) sender ! JobRefused(id)
        else {
          curJob = Some(newjob)
          curJobId = Some(id)
          server = Some(sender)
          sender ! JobAccepted(id)
          startJob
        }
      }

      case AskJob => {
        if (curJob.isDefined) sender ! OnJob(curJobId)
        else sender ! NoJob
      }

      case Quit => {
        workers foreach { w => w ! ClientInternalMessage.Quit }
        println("\007\b")
        exit()
      }

      case Hi => println("Connection up")
    }
  }

  /*
   * új job indítása a létező szálak között szétosztja a feladatot,
   * és ha nincs elég szál(nCpuCores) de kéne csinál még újakat
   */
  def startJob() = {
    import ClientInternalMessage._
    val job = curJob.get
    workers foreach { w =>
      if (!w.onJob && job.hasNext) {
        val nextjob = job.next()
        w ! AssignJob(nextId, nextjob)
        jobsWorking = nextId :: jobsWorking
        nextId += 1
      }
    }

    while (job.hasNext && workers.size < Runtime.getRuntime.availableProcessors) {
      val w = new Worker
      workers = w :: workers
      val nextjob = job.next()
      w ! AssignJob(nextId, nextjob)
      jobsWorking = nextId :: jobsWorking
      nextId += 1
      w.start()
    }
  }

  def handleInternal(msg: ClientInternalMessage): Unit = {
    import ClientInternalMessage._
    if (ClientEntry.verbose) println(msg)
    msg match {
      case JobResult(id, result) => {
        resultBuffer += result
        jobsWorking = jobsWorking diff id :: Nil
        if (curJob.isDefined) if (curJob.get.hasNext) {
          val nextjob = curJob.get.next()
          sender ! AssignJob(nextId, nextjob)
          jobsWorking = nextId :: jobsWorking
          nextId += 1
        }
        if (curJob.isDefined) {
          if (!hasJob && !curJob.get.hasNext) sendResult()
        } else if (!hasJob) sendResult()
      }
    }
  }

  def sendResult() = {
    import NetworkMessage._
    server.get ! JobResult(curJobId.get, resultBuffer.toArray)
    curJob = None
    curJobId = None
    server = None
    resultBuffer.clear()
    Runtime.getRuntime().gc()
  }

  def hasJob = jobsWorking.length != 0
}