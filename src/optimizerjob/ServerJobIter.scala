package optimizerjob
import server.Types._
import optimization._

class ServerJobIter(val params: ParamsIter, val cyclesperjob: Int) extends Iterator[(Id, Job)] {
  var curParams = params.next()
  var rTest = curParams.nTest
  var nextuid = 0
  override def next() = {
    val nTest = math.min(rTest, jobsPerClient)
    val ret = new WorkerJobIter(Params(nTest, curParams.nCycles, curParams.acceptRise, curParams.startState))
    rTest-=nTest
    if(rTest<=0 && params.hasNext){
      curParams=params.next()
      rTest=curParams.nTest
    }
    nextuid+=1
    (nextuid.toString+" "+ret.params.toString,ret)
  }

  def jobsPerClient(): Int = {
    def inner(n: Int): Int = {
      if (n * curParams.nCycles > cyclesperjob) {
        if (math.abs(n * curParams.nCycles - cyclesperjob) <
          math.abs((n - 1) * curParams.nCycles - cyclesperjob)) return n
        else return n - 1
      }
      inner(n + 1)
    }
    inner(2)
  }

  override def hasNext() = rTest>0
}