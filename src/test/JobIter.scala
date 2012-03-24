package test
import server.Types._
class JobIter extends Iterator[(Id, Job)] {
  var id = -1
  override def next(): (Id, Job) = synchronized{
    id += 1
    if(id>500) throw new Exception("Fuck")   
    (id, new JobIter2(id))
  }
  override def hasNext = id < 500
}