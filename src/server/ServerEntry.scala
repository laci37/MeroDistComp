package server
import Types._
import annotation.tailrec
import util.matching.Regex
object ServerEntry {
  def main(args: Array[String]) = {
    val jobsClass = Class.forName(args(0))
    val jobs = jobsClass.newInstance()
    val serv = new ServerControl(jobs.asInstanceOf[Iterator[(Id, Job)]])

    def procargs(i: Int) {
      if (i < args.length) {
        val Switch = new Regex("-[^o]+")
        args(i) match {
          case "-v" => {
            serv.verbose = true
            procargs(i + 1)
          }
          case "-o" => {
            if (args.length > i + 1){
            serv.outfile = args(i + 1)
            procargs(i+2)
            }
            else {
              println("missing argument after -o")
              exit()
            }
          }
        }
      }
    }

    procargs(1)
    //actors.Debug.level = 3
    serv.start
  }
}