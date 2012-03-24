package client
import actors.remote0._
object ClientEntry extends App with Global {
   if(args.length<3) {
     println("not enough args")
     exit()
   }
   RemoteActor.classLoader = new RemoteClassLoader(this.getClass.getClassLoader, hinode, Symbol(args(2)))
   val hinode=new Node(args(0),Integer.parseInt(args(1)))
   val hiserv= RemoteActor.select(hinode, Symbol(args(2)))
   val control=new ClientControl(1235,'client,hiserv)
   actors.Debug.level=3
   control.start()
}

trait Global{
  var verbose=true
  var classReqTimeOut=5000
}