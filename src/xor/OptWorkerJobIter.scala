package xor
import neural._
import opt._
import dsl._
import optimization._
class OptWorkerJobIter(val nTest: Int) extends Iterator[Unit => Any] {
  var rem = nTest
  val plan = (2 lin) >> (2 tanh) >> (1 tanh)
  
  def xorTest(n: Net): Double = {
    var score = 0d
    n.inputs(0) set 0d
    n.inputs(1) set 0d
    n.calc()
    score += math.pow(n.outputs(0).output + 1d, 2)
    n.inputs(0) set 0d
    n.inputs(1) set 1d
    n.calc()
    score += math.pow(n.outputs(0).output - 1d, 2)
    n.inputs(0) set 1d
    n.inputs(1) set 0d
    n.calc()
    score += math.pow(n.outputs(0).output - 1d, 2)
    n.inputs(0) set 1d
    n.inputs(1) set 1d
    n.calc()
    score += math.pow(n.outputs(0).output + 1d, 2)
    if (!(score.isNaN || score.isInfinity)) score else Double.PositiveInfinity
  }

  def next() = {
    rem -= 1
    def ret(a:Unit):Int = {
      var c=0
      val opt = new HeuristicRiseOptimizer(new WeightState(new Array[Double](9), plan, xorTest))
      while (opt.optimalCost>1e6) {
        opt.optimize(1)
        c+=1
      }
      c
    }
    ret
  }

  def hasNext() = rem > 0
}