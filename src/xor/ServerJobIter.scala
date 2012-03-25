package xor
import server.Types._
import optimization._

class ServerJobIter extends Iterator[(Id,Job)] {
  var c=0
  override def next():(Id,Job)={
    c+=1
    if(c<100) ("ga"+c, new GaWorkerJobIter(50))
    else ("opt"+c, new OptWorkerJobIter(50))
  }
  
  override def hasNext()={
    c<200
  }
}