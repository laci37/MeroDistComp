package knaptest
import optimizerjob._
import knapsack._
class TestIter extends ParamsIter {
  val rand = new util.Random(50)
  var i = 0
  var j = -1
  override def next() = {
    j += 1
    if (j > 10) {
      j = 0
      i += 1
    }
    Params(
      50,
      (1000d * math.pow(2, i)).toInt,
      math.pow(10, -5) * math.pow(2, j),
      new KnapState(50, itemList, Nil))
  }

  override def hasNext = i < 10 || j < 10

  val itemList = (for (i <- (1 to 50))
    yield new Item(rand.nextDouble * 10, rand.nextDouble)).toList
}