package sajob
import sa._
class WorkerJobIter(val params: Params) extends Iterator[(Unit => Any)] with Serializable {
  var rTests = params.nTests

  override def next(): (Unit => Any) = {
    rTests -= 1
    return { _1: Unit =>
      val opt = new Optimizer(params.prob)
      opt.optimize
    }
  }

  override def hasNext = rTests > 0
}