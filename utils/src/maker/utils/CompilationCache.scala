package maker.utils

import java.io.File
import java.io.{InputStream, OutputStream}
import java.io.{BufferedInputStream, BufferedOutputStream, FileInputStream, FileOutputStream}
import java.io.ByteArrayInputStream
import org.apache.commons.io.IOUtils

import maker.utils.FileUtils._

case class CacheRequest(hash:Hash, file:File)
trait PersistentCache {
  def contains(hash : Hash) : Boolean
  def get(hash: Hash):Option[List[String]]
  def read(request:Seq[CacheRequest])
  def put(hash:Hash, inputStream:InputStream)
  def put(requests:Seq[CacheRequest])
}
class FileSystemPersistentCache(cacheDir:File) extends PersistentCache {
  def contains(hash : Hash) : Boolean = fileForHash(cacheDir, hash).exists
  def get(hash: Hash):Option[List[String]] = {
    val listingFile = fileForHash(cacheDir, hash)
    if (listingFile.exists()) {
      val lines = listingFile.readLines.toList
      Some(lines)
    } else {
      None
    }
  }
  def read(requests:Seq[CacheRequest]) {
    requests.foreach { request => {
      val cacheFile = fileForHash(cacheDir, request.hash)
      if (!cacheFile.exists()) throw new Exception("No cache file found for " + request.hash)
      val fis = new FileInputStream(cacheFile)
      val fos = new FileOutputStream(request.file)
      IOUtils.copy(fis, fos)
      fis.close()
      fos.close()
    } }
  }

  def put(requests: Seq[CacheRequest]) {
    requests.foreach { request => {
      val fis = new FileInputStream(request.file)
      put(request.hash, fis)
      fis.close()
    } }
  }

  def put(hash: Hash, inputStream:InputStream) {
    val hashFile = fileForHash(cacheDir, hash)
    if (!hashFile.exists()) {
      val fos = new FileOutputStream(hashFile)
      IOUtils.copy(inputStream, fos)
      inputStream.close()
      fos.flush()
      fos.close()
    }
  }

  private def fileForHash(cacheDir:File, hash:Hash) = new File(
    cacheDir, hash.hashWithComment.substring(0, hash.hashWithComment.length min 20)
  )
}

class Files(persistentCache:PersistentCache, lines:List[String]) {
  def copyTo(root:File, dirs:File*) {
    CompilationCache.restore(persistentCache, root, lines, dirs.toList)
  }
}

object CompilationCache {

  def lookup(persistentCache:PersistentCache, sourceHash:Hash) = {
    persistentCache.get(sourceHash).map { lines =>
      new Files(persistentCache, lines)
    }
  }

  def save(persistentCache:PersistentCache, root:File, sourceHash:Hash, dirs:File*) {
    val listing = new StringBuilder()
    listing.append("# Hostname: " + "\n")
    listing.append("# Date: " + new java.util.Date() + "\n")
    listing.append("# Pwd: " + new File(".").getAbsoluteFile + "\n")
    var requests = List[CacheRequest]()
    dirs.flatMap(_.listAllFiles.sortBy(_.getName)).foreach { file => {
      if (file.isDirectory) {
        listing.append(file.relativeTo(root).getPath + "\n")
      } else {
        val fileHash = HashCache.hash(file)
        requests = CacheRequest(fileHash, file) :: requests
        listing.append(file.relativeTo(root).getPath + " " + fileHash.hashWithComment + "\n")
      }
    } }
    persistentCache.put(requests)
    persistentCache.put(sourceHash, new ByteArrayInputStream(listing.toString.getBytes("UTF8")))
  }

  def restore(persistentCache:PersistentCache, root:File, lines:List[String], dirs:List[File]) {
    val allFiles:List[(File,Option[Hash])] = lines.flatMap { line => {
      if (!line.startsWith("#")) {
        line.split(" ").toList match {
          case dirName :: Nil => {
            val dir = new File(root.getPath + "/" + dirName).getAbsoluteFile
            Some( (dir, None) )
          }
          case fileName :: hashText :: Nil => {
            val hash = Hash.fromHex(hashText)
            val file = new File(root.getPath + "/" + fileName).getAbsoluteFile
            Some( (file, Some(hash)))
          }
          case _ => throw new Exception("Unepected line format " + line)
        }
      } else {
        None
      }
    } }
    allFiles.foreach { case (dir, None) => dir.mkdirs(); case _ => }
    val cacheRequests = allFiles.collect {
      case (file, Some(hash)) if (!file.exists || HashCache.hash(file) != hash) => CacheRequest(hash, file)
    }
    persistentCache.read(cacheRequests)
    val surplace = (dirs.flatMap(_.listAllFiles.map(_.getAbsoluteFile)).toSet -- allFiles.map(_._1))
    surplace.filter(_.getName.endsWith(".class")).foreach(_.delete())
  }
}


