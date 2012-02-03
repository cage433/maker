def writeToFile(fileName : String, text : String){
  val fstream = new FileWriter(fileName)
  val out = new BufferedWriter(fstream)
  out.write(text)
  out.close()
}

def writeClasspath{
  val cp = launcher.compilationClasspath
  writeToFile("launcher-classpath.sh", "export STARLING_CLASSPATH=" + cp)
}
