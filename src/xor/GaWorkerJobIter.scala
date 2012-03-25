package xor
import nga._
import neural._
import neural.nga._
import neural.dsl._
class GaWorkerJobIter(val nTests: Int) extends Iterator[Unit => Any] {
  var rem = nTests
  val plan = (2 lin) >> (2 tanh) >> (1 lin)

  def xorTest(n: Net) = {
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
    if (!(score.isNaN || score.isInfinity)) math.pow(2, -score) else 0d
  }

  override def next() = {
    rem -= 1
    def ret(a: Unit) = {
      var c = 0
      val gen = new GenerationBase(for (i <- (1 to 20)) yield WeightGenome.zero(plan, xorTest)) with Elitism
      while (gen.best.fitness < 1 - 1e6) {
        gen.step()
        c += 1
      }
      c
    }
    ret
  }

  override def hasNext = {
    rem > 0
  }
}