package server
import actors._
object Types {
 type Client = OutputChannel[Any] //typedefs for better understanding
  type Id = Any
  type Job = Iterator[(Unit=>Any)]//[Unit => Any]
}