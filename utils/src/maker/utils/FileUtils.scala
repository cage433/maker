/*
 * Copyright (c) 2011-2012, Alex McGuire, Louis Botterill
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met: 
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution. 
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package maker.utils

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.BufferedReader
import java.io.FileReader
import maker.utils.os.Command
import maker.Props
import java.util.concurrent.atomic.AtomicBoolean
import org.apache.commons.io.{FileUtils => ApacheFileUtils}
import java.io.InputStreamReader
import java.io.FileInputStream

object FileUtils extends Asserting{

  def maybeFile(dir : File, basename : String) : Option[File] = {
    val f = file(dir, basename)
    if (f.exists)
      Some(f)
    else
      None
  }

  def file(f : String) : File = {
    assert(f != null)
    new File(f)
  }
  def file(f : File, d : String*) : File = {
    assert(f != null)  // Because Java will happily ignore a null f 
    d.toList match {
      case Nil => f
      case x::rest => file(new File(f, x), rest : _*)
    }
  }
  def file(base : String, file : String) : File = {
    assert(base != null && file != null)
    new File(base, file)
  }

  def cwd = file(System.getProperty("user.dir"))

  implicit def toRichFile(f : File) = RichFile(f)

  case class RichFile(plainFile : File) {

    def absPath : String = plainFile.getAbsolutePath

    // returns false if this file or the supplied dir does not exist, not sure if this is always
    //   what you might want so this comment is a warning
    def isContainedIn(dir : File) = {
      def recurse(f : File) : Boolean = {
        if (f == null) false
        else if (f == dir) true 
        else if (f == new File("/")) false 
        else recurse(f.getParentFile)
      }
      recurse(plainFile)
    }

    def readLines : List[String] = if (plainFile.exists) io.Source.fromFile(plainFile).getLines().toList else Nil

    def read : String = {
      val buffer = new StringBuffer
      val iStream = new InputStreamReader(new FileInputStream(plainFile))
      val tmp = new Array[Char](4096)
      while(iStream.ready) {
        val read: Int = iStream.read(tmp)
        buffer.append(tmp, 0, read)
      }
      iStream.close
      buffer.toString
    }

    def append(s:String) {
      val stringToWrite = {
        readLines.toList.lastOption match {
          case Some(l) if l.endsWith("\n") => "\n" + s + "\n"
          case _ => s + "\n"
        }
      }
      appendToFile(plainFile, stringToWrite)
    }

    def asAbsoluteFile = asserting[File](file(plainFile.getAbsolutePath), _.isAbsolute)

    def relativeTo(dir: File) : File = {
      def pathComponents(f : File) = {
        f.getAbsolutePath.split("/").filterNot{component =>  component == "" || component == "."}.toList
      }
      val dirComponents = pathComponents(dir)
      val fileComponents = pathComponents(plainFile)
      val commonComponents = dirComponents.zip(fileComponents).takeWhile{
        case (d, f) => d == f
      }.map(_._1)
      val directoriesUp = List.fill(dirComponents.size - commonComponents.size)("..")
      val relativeComponents = directoriesUp ::: fileComponents.drop(commonComponents.size)
      file(relativeComponents.mkString("/"))
    }
    def className(classDirectory : File) : String = {
      relativeTo(classDirectory).getPath.split('.').head.replace("/", ".")
    }
    def basename : String = plainFile.getName
    def basenameSansExtension : String = basename.split('.').head
    def isScalaFile = plainFile.getName.endsWith(".scala")
    def isJavaFile = plainFile.getName.endsWith(".java")
    def isClassFile = plainFile.getName.endsWith(".class")
    def isJar = plainFile.getName.endsWith(".jar")

    def deleteAll() : Unit = recursiveDelete(plainFile)
    def listAllFiles = allFiles(plainFile)

    def safeListFiles : List[File] = Option(plainFile.listFiles).toList.flatten

    def rich = this

    // Taken from http://www.4pmp.com/2009/12/java-touch-set-file-last-modified-time/
    def touch = {
      if (plainFile.exists) {
          if (!plainFile.setLastModified(System.currentTimeMillis)) 
              throw new Exception("Could not touch file " + plainFile)
      } else {
        createParentDir(plainFile)
        plainFile.createNewFile()
      }
      assert(plainFile.exists, plainFile + "should exist")
      plainFile
    }

    def asNewDirectory : File = {
      recursiveDelete(plainFile)
      plainFile.mkdir
      plainFile
    }

    def copyTo(file : File){
      if (file.isDirectory)
        ApacheFileUtils.copyFileToDirectory(plainFile, file)
      else
        ApacheFileUtils.copyFile(plainFile, file)
    }

    def makeDir() : File = FileUtils.mkdir(plainFile)
    def makeDirs() : File = {
      plainFile.mkdirs
      plainFile
    }
    def hash : String = {
      Hash.calculateHash(plainFile).hex
    }
    def subDirs : List[File] = safeListFiles.filter(_.isDirectory)

    def doesNotExist = !plainFile.exists
    def dirname = plainFile.getParentFile
  }

  def findFiles(pred : File => Boolean, dirs : Iterable[File]) : Iterable[File] = {
    def rec(file : File) : Iterable[File] = {
      if (file.isDirectory)
        file.listFiles.toSet.flatMap(rec)
      else if (pred(file))
        List(file)
      else
        Nil
    }
    dirs.flatMap(rec).map{f : File => f.asAbsoluteFile}
  }

  def findFilesWithExtension(ext : String, dirs : Iterable[File]) : Iterable[File] =
    findFiles(_.getName.endsWith("." + ext), dirs)

  def findFilesWithExtension(ext : String, dirs : File*) : Iterable[File] =
      findFilesWithExtension(ext, dirs.toSet)

  def findFilesWithExtensions(exts : List[String], dirs : Iterable[File]) =
    findFiles((f : File) => exts.exists(e => f.getName.endsWith("." + e)), dirs)

  def traverseDirectories(root : File, fn : File => Unit){
    fn(root)
    root.subDirs.foreach(traverseDirectories(_, fn))
  }

  def findInPaths(props : Props, paths : List[File], predicate : String => Boolean) : List[File] = {
    def find(p : List[File]) : List[File] = {
      p match {
        case Nil | null => Nil
        case x :: xs => x match {
          case z if z.isFile => z match {
            case f if (f.getName.endsWith(".jar")) => if (findInArchive(props, f, predicate)) f :: find(xs) else find(xs)
            case f => if (predicate(f.getName)) f :: find(xs) else find(xs)
          }
          case d if (d.isDirectory) => find(d.safeListFiles) ::: find(xs)
          case _ => find(xs)
        }
      }
    }
    find(paths)
  }
  /// extracts the table of contents for the archive and looks for a matching line
  def findInArchive(props : Props, file : File, predicate : String => Boolean) : Boolean = {
    import maker.utils.os._
    val coh = new CommandOutputHandler(None, Some(new StringBuffer()), false)
    val cmd = new Command(props, coh, None, props.Jar, "-tf", file.getAbsolutePath)
    cmd.exec() match {
      case 0 => cmd.savedOutput.split('\r').exists(predicate)
      case _ => false
    }
  }

  /// recursively enumerate all files within a given dir (returns the supplied file if it's not a directory)
  def allFiles(f : File) : List[File] =
    if (f.isDirectory)
      f :: Option(f.listFiles).map(_.toList.flatMap(allFiles)).getOrElse(Nil)
    else f :: Nil

  def lastModifiedFileTime(files : Iterable[File]) =
    files.toList.flatMap(allFiles).map(_.lastModified).sortWith(_ > _).headOption

  def lastModifiedProperFileTime(files : Iterable[File]) =
    files.toList.flatMap(allFiles).filterNot(_.isDirectory).map(_.lastModified).sortWith(_ > _).headOption

  def lastModifiedFile(files : Iterable[File]) =
    files.toList.flatMap(allFiles).sortWith(_.lastModified > _.lastModified).headOption
  
  def fileIsLaterThan(target : File, dirs : List[File]) = {
    target.exists() && (target.lastModified >= lastModifiedFileTime(dirs).getOrElse(0L))
  }

  def replaceInFile(file : File, placeholder : String, repl : String) = {
    val lines = file.readLines.map(_.replace(placeholder, repl))
    writeToFile(file, lines.mkString("", "\n", "\n"))
  }

  def nameAndExt(file : File) = {
    val name = file.getName
    file.getName.lastIndexOf('.') match {
      case -1 => (name, "")
      case n => val parts = name.splitAt(n); (parts._1, parts._2.substring(1))
    }
  }

  def findJars(dir : File) : Iterable[File] = findJars(List(dir))
  def findJars(dirs : Iterable[File]) = findFilesWithExtension("jar", dirs)
  def findClasses(dir : File) = findFilesWithExtension("class", Set(dir))

  /**
   * Don't want to use PrintWriter as that swallows exceptions
   */
  case class RichBufferedWriter(writer : BufferedWriter){
    def println(text : String){
      writer.write(text)
      writer.newLine
    }
  }
  implicit def toRichBufferedWriter(writer : BufferedWriter) = RichBufferedWriter(writer)

  private def createParentDir(file : File){
    if (file.getParentFile != null && !file.getParentFile.exists)
      file.getParentFile.mkdirs
  }
  def withFileWriter(file : File)(f : BufferedWriter => _){
    createParentDir(file)
    val fstream = new FileWriter(file)
    val out = new BufferedWriter(fstream)
    try {
      f(out)
    } finally {
      out.close()
    }
  }

  def withFileReader[T](file : File)(f : BufferedReader => T) = {
    createParentDir(file)
    val in = new BufferedReader(new FileReader(file))
    try {
      f(in)
    } finally {
      in.close()
    }
  }

  def withFileAppender(file : File)(f : BufferedWriter => _){
    createParentDir(file)
    val fstream = new FileWriter(file, true)
    val out = new BufferedWriter(fstream)
    try {
      f(out)
    } finally {
      out.close()
    }
  }

  def withFileLineReader[A](file : File)(f : String => A) = {
    var res : List[A] = Nil
    if (file.exists()) {
      withFileReader(file){
        in : BufferedReader => 
          var line = in.readLine
          while (line != null) {
            res = f(line) :: res
            line = in.readLine
          }
      }
    }
    res.reverse
  }
  
  def tempDir(name : String = "makerTempFile") = {
    val temp = File.createTempFile(name, java.lang.Long.toString(System.nanoTime))
    recursiveDelete(temp)
    temp.mkdir
    temp
  }

  def cleanRegularFilesLeavingDirectories(file : File){
    if (file.exists && file.isDirectory){
      Option(file.listFiles).getOrElse(Array()).foreach(cleanRegularFilesLeavingDirectories)
    } else {
      file.delete
    }
  }

  def recursiveDelete(file : File){
    if (file.exists && file.isDirectory){
      Option(file.listFiles).getOrElse(Array()).foreach(recursiveDelete)
      file.delete
    } else
      file.delete
  }

  def writeToFile(file : File, text : String)  = {
    withFileWriter(file){
      out : BufferedWriter =>
        out.write(text)
    }
    file
  }

  def appendToFile(file : File, text : String){
    withFileAppender(file){
      out : BufferedWriter =>
        out.write(text)
    }
  }

  def withTempFile[A](f : File => A, deleteOnExit : Boolean = true) = {
    
    val file = File.createTempFile("makerTempFile", java.lang.Long.toString(System.nanoTime))
    val result = try {
      f(file)
    } finally {
      if (deleteOnExit)
        recursiveDelete(file)
    }
    result
  }


  def withTempDir[A](f : File => A) = {
    withTempFile(
      {file : File => 
        recursiveDelete(file)
        file.mkdir
        f(file)
      }, 
      deleteOnExit = true
    )
  }

  /**
    * Intended to be swapped in for withTempDir as a convenience
    * when analysing broken unit testa
    */
  private val testDirBeingUsed = new AtomicBoolean(false)
  def withTestDir[A](f : File => A) = {
    assert(testDirBeingUsed.compareAndSet(false, true), "Test directory used by another test - this will end badly")
    val dir = file("/tmp/makerTestDir")
    dir.mkdir
    dir.listFiles.foreach(recursiveDelete(_))
    f(dir)
  }

  def extractMapFromFile[K, V](file : File, extractor : String => (K, V)) : Map[K, V] = {
    var map = Map[K, V]()
    if (file.exists) {
      withFileLineReader(file) {
        line : String => 
          map += extractor(line)
      }
    }
    map
  }

  def writeMapToFile[K, V](file : File, map : scala.collection.Map[K, V], fn : (K, V) => String){
    withFileWriter(file) {
      out: BufferedWriter =>
        map.foreach {
          case (key, value) =>
            out.println(fn(key, value))
        }
    }
  }

  def mkdirs(dir : File, subdirs : String*) : File = {
    val dir_ = file(dir, subdirs : _*)
    dir_.mkdirs
    dir_
  }

  def mkdir(dir : File) : File = {
    if (!dir.exists)
      assert(dir.mkdir, "Couldn't create " + dir)
    assert(dir.isDirectory, "Couldn't create directory " + dir + " as there is a proper file of that name")
    dir
  }
  def mkdir(dir : File, name : String) : File = {
    mkdir(file(dir, name))
  }
}
