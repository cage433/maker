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

package maker.utils.ivy

import java.io.File
import scala.xml.XML._
import maker.utils._
import maker.utils.Utils._
import maker.utils.maven._
import maker.utils.ModuleId._
import maker.MakerProps

/**
 * Read in raw ivy xml files for module dependencies
 */
case object IvyReader {

  /// extract dependencies directly from ivy files
  def readIvyDependenciesFromFile(log : MakerLog, file : File, props : Map[String, String]) : List[DependencyLib] = {
    try {
      val ivyXml = synchronized { loadFile(file) }
      val deps = (ivyXml \\ "dependency")

      val subsIvyProps = subs(props ++ sysProps) _

      deps.map(d => {
        val name = subsIvyProps((d \ "@name").toString)
        val org = subsIvyProps((d \ "@org").toString)
        val rev = (d \ "@rev").toList.headOption.map(r => subsIvyProps(r.toString))

        val artifactDetails = (d \ "artifact").toList.headOption

        val (typ, ext, classifier) = artifactDetails match {
          case None => (None, None, None)
          case Some(ad) => {
            val t = (ad \ "@type").toList.headOption.map(v => MavenNamedType(subsIvyProps(v.toString)))
            val e = (ad \ "@ext").toList.headOption.map(v => MavenNamedType(subsIvyProps(v.toString)))
            val c = (ad \ "@classifier").toList.headOption.map(v => MavenNamedClassifier(subsIvyProps(v.toString)))
            (t, e, c)
          }
        }

        DependencyLib(name, org % name % rev, CompileScope, None, typ, classifier)
      }).toList
    }
    catch {
      case _ : java.io.FileNotFoundException => Nil // assume no file = no deps, for now
      case th : Throwable => {
        log.error("Error reading XML file " + file.getAbsolutePath, th)
        log.info("File was:")
        val f = scala.io.Source.fromFile(file)
        f.getLines.foreach(l => log.info(l))
        throw th
      }
    }
  }

  /// extract (ibiblio) resolvers directly from ivy files
  def readIvyResolversFromFile(log : MakerLog, file : File) : List[MavenRepository] = {
    try {
      val ivyXml = synchronized { loadFile(file) }
      val repos = (ivyXml \\ "ibiblio")
      repos.map(r => {
        val id = (r \\ "@name").toString
        val url = (r \\ "@root").toString
        MavenRepository(id, id, url, "default")
      }).toList
    }
    catch {
      case _ : java.io.FileNotFoundException => Nil // assume no file = no deps, for now
      case th : Throwable => {
        log.error("Error reading XML file " + file.getAbsolutePath, th)
        throw th
      }
    }
  }

  /// extract ivy properties from ivy settings file
  // this will also substitute system properties for unresolved properties
  def readIvyPropertiesFromFile(log : MakerLog, file : File) : Map[String, String] = {
    try {
      val ivyXml = synchronized { loadFile(file) }
      val props = (ivyXml \\ "property")
      props.map(r => {
        val n = (r \\ "@name").toString
        val v = subsWithSysProps((r \\ "@value").toString)
        n -> v
      }).toMap
    }
    catch {
      case _ : java.io.FileNotFoundException => { // assume no file = no deps, for now
        log.warn("no ivy settings at " + file.getAbsolutePath)
        Map()
      }
      case th : Throwable => {
        log.error("Error reading XML file " + file.getAbsolutePath, th)
        throw th
      }
    }
  }

  def sanitiseName(name : String) = {
    import scala.util.matching.Regex
    val nonWordPattern = new Regex("\\W")
    nonWordPattern.replaceAllIn(name, "_")
  }
  def propertyName(groupAndArtifact : GroupAndArtifact) = sanitiseName("m_" + groupAndArtifact.toString + "_version")

  /// extract ivy version properties from ivy module file
  def extractIvyPropertiesFromFile(log : MakerLog, file : File) : Map[String, String] = {
    val deps = readIvyDependenciesFromFile(log, file, Map())
    //println("deps read = ")
    deps.foreach(println)
    val pred = (d : DependencyLib) => d.gav.version.map(v => !v.version.contains("S")).getOrElse(false)
    val x = deps.filter(pred).flatMap(d => d.gav.version.map(v => propertyName(d.gav.groupAndArtifact) -> v.version)).toMap
    //println("\n" + x.mkString("\n"))
    x
  }
}
