package test
import server.Types._
class JobIter2(val a: Int) extends Iterator[(Unit=>Any)] with Serializable {
  var c = 0
  override def next(): (Unit=>Any) = synchronized {
    println("next1 " + c)
    c += 1
    println("next2 " + c)
    val x:Int=c //needed to bypass pass by reference
    return { b: Unit => a + x - 1 }
  }
  override def hasNext = { c <= 50 }
}