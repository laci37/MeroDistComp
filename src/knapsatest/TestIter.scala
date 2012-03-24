package knapsatest

import sajob._
import knapsa._
class TestIter extends ParamsIter {
  var i = -1
  val rand = new util.Random(50)
  val itemSet = (for (i <- (1 to 50))
    yield new Item(rand.nextDouble * 10, rand.nextDouble)).toList

  override def next() = {
    i += 1
    new Params(50, new SqKnapProblem(50, itemSet, (1000d * math.pow(2, i)).toInt))
  }

  override def hasNext() = i < 10
}