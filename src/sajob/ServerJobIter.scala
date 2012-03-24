package sajob
import server.Types._
import sa._
class ServerJobIter(params: ParamsIter, cyclesperjob: Int) extends Iterator[(Id, Job)] {
  var curParams = params.next()
  var rTests = curParams.nTests
  var nextuid = 0

  override def next() = {
    val nTests = math.min(rTests, jobsPerClient)
    val ret = new WorkerJobIter(Params(nTests, curParams.prob))
    rTests -= nTests
    if (rTests <= 0 && params.hasNext) {
      curParams = params.next()
      rTests = curParams.nTests
    }
    nextuid += 1
    (nextuid + " " + ret.params, ret)
  }

  override def hasNext = rTests > 0

  def jobsPerClient(): Int = {
    def inner(n: Int): Int = {
      if (n * curParams.prob.cycles > cyclesperjob) {
        if (math.abs(n * curParams.prob.cycles - cyclesperjob) <
          math.abs((n - 1) * curParams.prob.cycles - cyclesperjob)) return n
        else return n - 1
      }
      inner(n + 1)
    }
    inner(2)
  }
}