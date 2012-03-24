package tspsatest
import sajob._
import tspsa._
import tsp.{ City, CityGraph }
import collection.mutable.Map
class TestIter extends ParamsIter {
  var i = -1
  
  val graph = new CityGraph((for (i <- (0 to 49)) yield (new City(Map.empty[Int, Double], i))).toList)
  for (i <- (0 to 48)) for (j <- (1 to 49)) if (j > i) {
    graph.setDist(i, j, math.round(20d * math.abs(math.sin(i + j))).toInt)
  }
  
  override def next() = {
      i += 1
      Params(50, new SqTspProblem(graph, (1000d * math.pow(2, i)).toInt))
    }

  override def hasNext = i < 10

}