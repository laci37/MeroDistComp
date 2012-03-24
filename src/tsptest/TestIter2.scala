package tsptest
import optimizerjob._
import tsp._
import collection.mutable.Map
class TestIter2 extends ParamsIter {
  var i = 0
  var j = -1
  val graph = new CityGraph((for (i <- Range(0, 50)) yield new City(Map.empty[Int, Double], i)).toList)
  for (i <- (0 to 48)) for (j <- (1 to 49)) if (j > i) {
    graph.setDist(i, j, math.round(20d * math.abs(math.sin(i + j))).toInt)
  }

  override def next() = {
    j += 1
    if (j > 10) {
      j = 0
      i += 1
    }
    Params(
      50,
      (1000d * math.pow(2, i)).toInt,
      0.01d * math.pow(3, j),
      new TSPState(graph.cities, graph))
  }
  override def hasNext = i < 7 || j < 10
}