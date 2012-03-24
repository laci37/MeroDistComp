package server
import java.io._
object FileClassLoader extends ClassLoader {

  override def loadClass(name: String): Class[_] = {
    var c = findLoadedClass(name)
    if (c == null) {
      try {
        c = findSystemClass(name)
      } catch { case _ => null }
    }
    if (c == null) {
      val filename = name.replace('.', File.separatorChar) + ".class";
      try {
        val data: Array[Byte] = loadClassData(filename);
        c = defineClass(name, data, 0, data.length);
        if (c == null)
          throw new ClassNotFoundException(name);
      } catch {
        case _ => throw new ClassNotFoundException("Error reading file: " + filename);
      }
    }
    resolveClass(c)
    c
  }
  
  def loadClassData(filename:String)={
    val f= new File(filename)
    val stream = new DataInputStream(new FileInputStream(f))
    var buffer=new Array[Byte](f.length.toInt)
    stream.readFully(buffer)
    buffer
  }
  
  def loadClassFromFile(filename:String):Class[_]={
    val data=loadClassData(filename)
    val c=defineClass(null,data,0,data.length)
    resolveClass(c)
    c
  }
}