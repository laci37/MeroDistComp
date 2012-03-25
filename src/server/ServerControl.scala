package server
import Types._
import actors._
import actors.remote0.RemoteActor._
import collection.mutable.Map
import msg._
class ServerControl(val jobs: Iterator[(Id, Job)]) extends Actor {
  var clients: List[Client] = Nil
  var jobsPending = Map.empty[Id, (Job, Int)]
  var jobsRefused = Map.empty[Id, Job]
  var jobsWorking = Map.empty[Id, (Job,Client)]
  var outfile = "output.txt"
  var verbose = false

  val classLoadAct = new ClassLoaderActor
  val saver = new Saver(outfile)
  classLoader = this.getClass.getClassLoader

  def act() = {
    println("\007")
    alive(12345)
    register('Server, this)
    saver.start()
    classLoadAct.start()
    loop {
      receive {
        case msg: NetworkMessage => handleNetwork(msg)
        case msg: ServerInternalMessage => handleInternal(msg)
      }
    }
  }

  def handleNetwork(msg: NetworkMessage) = {
    import NetworkMessage._
    if(verbose) println(msg)
    msg match {
      case Hi => {
        if(!clients.contains(sender)){
        println("New Client")
        clients = sender :: clients
        sender ! Hi
        assignJob(sender)
        }
      }
      case JobAccepted(id) => {
        if (jobsPending.contains(id)) {
          jobsWorking +=((id,(jobsPending(id)._1,sender)))
          jobsPending = jobsPending - id 
        }
        else if (jobsRefused.contains(id)) {
          jobsWorking +=((id,(jobsRefused(id),sender)))
          jobsRefused -= id
        }
        else {
          sender ! ExitJob
          assignJob(sender)
        }
        
      }
      case JobRefused(id) => {
        if (jobsPending.contains(id)) {
          jobsRefused += id -> jobsPending(id)._1
          jobsPending = jobsPending - id
        }
      }
      case JobResult(id, result) => {
        saver ! ServerInternalMessage.SaveResult(id, result)
        jobsWorking -= id
        assignJob(sender)
      }
      case ClassRequest(name) => {
        classLoadAct ! ServerInternalMessage.SendClassData(name, sender)
      }
    }
  }

  def handleInternal(msg: ServerInternalMessage) = {
    import ServerInternalMessage._
    if(verbose) println(msg)
    msg match {
      case CyclicCheck => {
        val curTime = System.currentTimeMillis / 1000
        jobsPending foreach { kv =>
          if (curTime - kv._2._2 > 60) {
            jobsRefused += kv._1 -> kv._2._1
            jobsPending -= kv._1
          }
        }
      }
    }
  }

  def assignJob(c: Client) {
    import msg.NetworkMessage._
    val newjob = (
      if (jobsRefused.size > 0) {
        val ret = jobsRefused.head
        jobsRefused = jobsRefused.tail
        Some(ret)
      } else if(jobs.hasNext) 
        Some(jobs.next())
      else None)
    if(newjob.isDefined){
    c ! AssignJob(newjob.get._1, newjob.get._2)
    jobsPending += newjob.get._1 -> (newjob.get._2, (System.currentTimeMillis() / 1000).toInt)
    }
    else {
      println("No job to give")
      if(jobsWorking.size==0) {
        println("All Jobs done")
        clients foreach {c => c ! Quit}
        saver ! ServerInternalMessage.Quit
        println("\007")
        exit()
      }
    }
  }
}