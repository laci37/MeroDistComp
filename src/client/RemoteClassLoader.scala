package client
import actors._
import remote0._
class RemoteClassLoader(defaultloader: ClassLoader, serv: => Node, sym: Symbol) extends ClassLoader {
  if (ClientEntry.verbose) println("RemoteClassLoader created")
  override def loadClass(name: String): Class[_] = {
    if (ClientEntry.verbose) println("loadClass " + name)
    var c = findLoadedClass(name)
    if (name(0) == '[') c = loadNativeArray(name)
    if (c == null) {
      try {
        c = findSystemClass(name)
      } catch { case _ => null }
    }
    if (c == null) {
      try {
        c = defaultloader.loadClass(name)
      } catch {
        case _ => c = loadRemoteClass(name)
      }
    }
    resolveClass(c)
    c
  }

  def loadRemoteClass(name: String): Class[_] = {
    val comm = new ClassRequester(serv, sym, name)
    comm.start()
    while (comm.data.isEmpty) { if (comm.err) throw new ClassNotFoundException }
    try {
      if (ClientEntry.verbose) println("Try to define remote class: " + name)
      defineClass(name, comm.data.get, 0, comm.data.get.length)
    } catch {
      case _ => throw new ClassNotFoundException(name)
    }
  }

  def loadNativeArray(name: String): Class[_] = {
    import java.lang.reflect.Array
    import util.matching.Regex
    val ArrayRE=new Regex("""\[(.*)""")
    val ClassRE=new Regex("""L(.*);""")
    name match {
      case ArrayRE(str) => {
        val inner = loadNativeArray(str)
        Array.newInstance(inner, 0).getClass
      }
      case ClassRE(str) => {
        loadClass(str)
      }
      case "B" => classOf[Byte]
      case "C" => classOf[Char]
      case "D" => classOf[Double]
      case "F" => classOf[Float]
      case "I" => classOf[Int]
      case "J" => classOf[Long]
      case "Z" => classOf[Boolean]
    }
  }
}