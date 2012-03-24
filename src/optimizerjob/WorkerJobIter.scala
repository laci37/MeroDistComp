package optimizerjob
import optimization._
class WorkerJobIter(val params: Params) extends Iterator[(Unit => Any)] with Serializable {
  var rTests = params.nTest
  override def next: (Unit => Any) = {
    rTests -= 1
    return { _1:Unit=>
    	val opt=new Optimizer(params.startState,params.acceptRise)
    	opt.optimize(params.nCycles)
    	opt.optimum
    }
  }

  override def hasNext = rTests>0
}