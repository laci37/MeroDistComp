package server

import actors.Actor
import Types._
import java.io._

class ClassLoaderActor extends Actor {

  def act() = {
    import ServerInternalMessage._
    loop {
      receive {
        case SendClassData(name, to) => {
          println(to+ " wants class "+name)
          to ! findClassData(name)
        }
      }
    }
  }

  def findClassData(name: String): msg.NetworkMessage = {
    import msg.NetworkMessage._
    println("Searching for class " + name)
    val stream = try {
      Class.forName(name).getClassLoader.getResourceAsStream(name.replace('.',File.separatorChar)+".class")
    } catch {
      case _ => null
    }
    
    if (stream != null) {
      import collection.mutable.ArrayBuffer
      val buffer = new ArrayBuffer[Byte]
      var len: Int = 1
      while (len > 0) {
        var bytes = new Array[Byte](1024)
        len = stream.read(bytes, 0, 1024)
        bytes = bytes.slice(0, len)
        buffer ++= bytes
      }
      ClassInfo(name, buffer.toArray)
    } else {
      println("Class " + name + " not in cache")
      try {
        var filename = name.replace('.', File.separatorChar) + ".class";
        var f = new File(filename)
        if (f exists) return ClassInfo(name, loadFromFile(f))
        filename = "bin" + File.separatorChar + filename
        f = new File(filename)
        if (f exists) return ClassInfo(name, loadFromFile(f))
        val s = filename.split(File.separatorChar)
        filename = s(s.length - 1)
        f = new File(filename)
        if (f exists) return ClassInfo(name, loadFromFile(f))
      }
      println("Class " + name + " not found")
      ClassNotFound
    }
  }

  def loadFromFile(f: File): Array[Byte] = {
    val stream = new DataInputStream(new FileInputStream(f))
    var buffer = new Array[Byte](f.length.toInt)
    stream.readFully(buffer)
    buffer
  }
}