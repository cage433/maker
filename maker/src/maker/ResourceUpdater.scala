package maker

import com.typesafe.config.Config
import maker.utils.http.HttpUtils
import maker.utils.FileUtils._
import java.io.{File, IOException}
import scala.collection.JavaConversions._
import org.apache.commons.io.{FileUtils => ApacheFileUtils}

//class ResourceUpdater(
  //resource : Resource, 
  //config : Config, 
  //directory : File
//)
  //extends ConfigPimps
//{

  //import HttpUtils.{StatusCode, ErrorMessage}

  //val resourceFile = file(directory, resource.basename)
  //resourceFile.dirname.makeDirs

  //def download() : Either[List[(StatusCode, ErrorMessage)], Unit] = {
    
    //var errors : List[(StatusCode, ErrorMessage)] = Nil
    //val downloaded = config.resolvers.foldLeft(false){
      //case (false, nextResolver)  => 
        //val url = nextResolver + "/" + resource.relativeURL
        //new HttpUtils().downloadFile(url, resourceFile) match {
          //case Left((statusCode, errorMessage)) => 
            //errors ::= (statusCode, errorMessage)
            //false
          //case Right(_) => 
            //true
        //}
      //case (true, _) => true
    //} 
    //if (downloaded)
      //Right(Unit)
    //else
      //Left(errors)
  //}

  /**
   * If the resource is not already in lib_managed (or equivalent) then try to copy from cache,
   * else download from external repository and put into cache.
   *
   * Note that source jars are only downloaded when we download a binary - this is as some
   * simply don't exist, and trying to download them every time we get an update can become
   * expensive if there are Nexus problems
   */
  //def update() : Resource.UpdateResult = {

    //val cachedFile = file(config.resourceCache, resourceFile.basename)
    
    //if (resourceFile.doesNotExist && cachedFile.exists){
      //ApacheFileUtils.copyFileToDirectory(cachedFile, resourceFile.dirname)
    //}

    //if (resourceFile.exists){
      //ResourceAlreadyExists
    //} else {
      //download() match {
        //case Right(_) => 
          //cacheResourceFile()
          //ResourceDownloaded
        //case Left(errors) => 
          //ResourceFailedToDownload(errors)
      //}
    //}
  //}
  //private def cacheResourceFile(){
    //try {
      //ApacheFileUtils.copyFileToDirectory(resourceFile, config.resourceCache)
    //} catch {
      //case _ : IOException => 
    //}
  //}

//}
