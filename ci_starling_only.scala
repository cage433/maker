import Common._
import Utils._
import Starling._

import maker.task.TaskFailed
import maker.task.TaskSucceeded
import maker.task.Dependency
import maker.project._
import maker.project.TopLevelProject
import maker.utils.FileUtils._
import maker.utils.MakerLog
import maker.utils.os.Command
import maker.utils.os.Command._
import maker.utils.ModuleId._
import maker.utils.GroupAndArtifact
import maker.task.BuildResult
import maker.task.Task
import maker.task.tasks._
import maker.utils._

import java.util.Properties
import java.io.File
import org.apache.commons.io.FileUtils._


println("loading ci_starling_only build file (script)...")

val buildType = getPropertyOrDefault("build.type", "starling")
val buildNo = getProperty("build.number")
val publishingResolverName = getPropertyOrDefault("publishing.resolver", "starling-snapshot")

try {
  (getProperty("git.commit"), getProperty("jenkins.job"), buildNo) match {
    case (Some(commit), Some(jobName), Some(v)) => {
      new redis.clients.jedis.Jedis("jenkins-starling").hset(commit, jobName, v)
      println(commit + " assigned to build " + jobName + " " + v)
    }
    case _ => println("Need build number, jenkins job and git commit to populate redis lookup. Skipping")
  }
} catch {
  case e:Exception => {
    println("Redis publish build update failed, but continuing...")
    e.printStackTrace()
  }
}

def doDocs(br : BuildResult) : BuildResult = {
  if (buildType == "starling") {
    println("generating documentation")
    br.flatMap(_ => starlingDTOApi.doc) // build an aggregated doc of the starling api, unfortunately can't build whole docs as arg list is too big!
  }
  else {
    println("not generating documentation")
    br
  }
}

println("finished loading definitions, starling build...")

val buildResults : BuildResult = for {
  _ <- starling.clean
  _ <- starling.update
  i1 <- launcher.test
  r <- doDocs(i1) //starlingDtoApi.docOnly(true) // build an aggregated doc of the starling api, unfortunately can't build whole docs as arg list is too big!
} yield r

writeStarlingClasspath

// only publish starling binary artefacts if we've a version number and the build.type = "starling"
val results : BuildResult = buildNo match {
  case Some(ver) if (buildType == "starling" && publishingResolverName == "starling-release") => {
    println("publishing starling as version " + ver + " to " + publishingResolverName)
    services.runMain("starling.services.WriteGitInfo")()()
    buildResults.flatMap(b => starling.Publish(resolver = publishingResolverName, version = ver).execute)
  }
  case _ => {
    println("skipping publishing, need a version number and build.type to be starling for starling publishing to work")
    buildResults
  }
}

println("build complete")
handleExit(results)
