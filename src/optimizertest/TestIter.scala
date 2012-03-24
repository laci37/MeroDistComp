package optimizertest
import optimizerjob.{ParamsIter,Params}
import knapsack._
class TestIter extends ParamsIter{
	var i=15
	override def next()={
	  i+=1
	  Params(
	      50,
	      50000,
	      i/4,
	      new KnapState(
	          i/2,
	          itemList(i),
	          Nil
	          )
	  )
	}
	
	def hasNext()= i<20
	
	def itemList(nItems:Int)={
	  val rand=new util.Random(nItems)
	  (for(i<-(1 to nItems)) 
      yield new Item(rand.nextDouble*10,rand.nextDouble)).toList
	}
}