package optimizerjob
import optimization._
abstract class ParamsIter extends Iterator[Params]
case class Params(val nTest:Int, val nCycles:Int, val acceptRise:Double, val startState:State) 