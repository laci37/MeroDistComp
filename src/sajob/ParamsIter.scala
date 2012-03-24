package sajob
import sa._
abstract class ParamsIter extends Iterator[Params]
case class Params(nTests:Int,prob:Problem)