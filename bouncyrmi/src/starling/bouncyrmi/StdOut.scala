package starling.bouncyrmi

object StdOut {
	val tee = new TeeOutputStream(System.out)
	private val nullAction = { (line:String)=>{} }
	def init = {
		System.setOut(new java.io.PrintStream(tee))
	}
	
	def setTee(action:String=>Unit) {
		tee.setAction(action)
	}
	def reset() {
		tee.setAction(nullAction)
	}
	
	def mainx(args:Array[String]) {
		init
		(1 to 4).foreach{ n => {
			new Thread(new Runnable() { def run() {
				val buffer = new StringBuilder
				setTee((line)=>buffer.append(line))
				println("OK")
				reset
				println(n + "Buffer: " + buffer)
			} }).start()
		}}
		
	}
}
import java.io._
class TeeOutputStream(chained:OutputStream) extends OutputStream {
  
	private val threadAction = new ThreadLocal[String=>Unit] () {
        override def initialValue() = {
            (line:String)=>{}
        }
    }
	private val threadBuffer = new ThreadLocal[StringBuffer] () {
        override def initialValue() = new StringBuffer
    }
	
	def setAction(action:String=>Unit) {
		threadAction.set(action)
	}
	
	override def write(c:Int) {
        chained.write(c)
//new Exception("J").printStackTrace(new PrintStream(chained))        
val buffer = threadBuffer.get
        if (c == '\n') {
        	threadAction.get()(buffer.toString)
        	buffer.setLength(0)
        } else {
        	buffer.append(c.asInstanceOf[Char])
        }
        flush()
    }

	override def flush() {
    	chained.flush()
    }
}
  
