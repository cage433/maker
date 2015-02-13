#!/usr/bin/python

import logging
import argparse
import os.path
import os
import shutil
from urllib import pathname2url
from urlparse import urljoin
from urllib2 import urlopen, URLError, HTTPError, Request
import tempfile
import signal
from subprocess import call
from glob import glob

# Computer generated section
MAKER_VERSION       = "0.03"
MAKER_SCALA_VERSION = "2.10.4"
SONATYPE            = "http://oss.sonatype.org/content/repositories/releases/"
MAVEN               = "http://repo1.maven.org/maven2/"
TYPESAFE            = "http://repo.typesafe.com/typesafe/releases/"
SCALA_RESOURCES     = [ (TYPESAFE, "org.scala-lang", "scala-library", MAKER_SCALA_VERSION),
                        (TYPESAFE, "org.scala-lang", "jline", MAKER_SCALA_VERSION),
                        (TYPESAFE, "org.scala-lang", "scala-compiler", MAKER_SCALA_VERSION),
                        (TYPESAFE, "org.scala-lang", "scala-reflect", MAKER_SCALA_VERSION)]
MAKER_RESOURCES     = [ (MAVEN, "org.scalatest", "scalatest_2.10", "2.2.0"),
                        (MAVEN, "ch.qos.logback", "logback-classic", "1.0.6"),
                        (MAVEN, "ch.qos.logback", "logback-core", "1.0.6"),
                        (MAVEN, "org.slf4j", "slf4j-api", "1.6.1"),
                        (MAVEN, "org.slf4j", "jcl-over-slf4j", "1.6.1"),
                        (MAVEN, "commons-codec", "commons-codec", "1.6"),
                        (MAVEN, "commons-io", "commons-io", "2.1"),
                        (MAVEN, "commons-pool", "commons-pool", "1.5.5"),
                        (MAVEN, "com.typesafe.sbt", "incremental-compiler", "0.13.5"),
                        (MAVEN, "com.typesafe.zinc", "zinc", "0.3.5"),
                        (MAVEN, "org.apache.commons", "commons-lang3", "3.1"),
                        (MAVEN, "org.apache.httpcomponents", "httpclient", "4.3"),
                        (MAVEN, "org.apache.httpcomponents", "httpcore", "4.3"),
                        (MAVEN, "org.apache.ivy", "ivy", "2.3.0-rc2"),
                        (MAVEN, "org.scalaz", "scalaz-core_2.10", "7.0.1"),
                        (MAVEN, "org.eclipse.aether", "aether-api", "1.0.1.v20141111"),
                        (MAVEN, "org.eclipse.aether", "aether-util", "1.0.1.v20141111"),
                        (MAVEN, "org.eclipse.aether", "aether-impl", "1.0.1.v20141111"),
                        (MAVEN, "org.eclipse.aether", "aether-connector-basic", "1.0.1.v20141111"),
                        (MAVEN, "org.eclipse.aether", "aether-transport-file", "1.0.1.v20141111"),
                        (MAVEN, "org.eclipse.aether", "aether-transport-http", "1.0.1.v20141111"),
                        (MAVEN, "io.spray", "spray-json_2.10", "1.3.1"),
                        (SONATYPE, "com.github.cage433", "maker", MAKER_VERSION),
                        (SONATYPE, "com.github.cage433", "maker-test-reporter", MAKER_VERSION)
                        ]
# End computer generated section


def read_args():
    global args
    parser = argparse.ArgumentParser()

    parser.add_argument('-r', '--refresh-maker-dependencies', action='store_true', dest='refresh', default=False)
    parser.add_argument('-p', '--project-definition-source-dir', dest='project_src_dir')
    args = parser.parse_args()

def create_logger():
    global log
    logging.basicConfig( \
            format= "%(asctime)-15s %(levelname)-10s Maker: %(message)s", \
            datefmt="%Y-%m-%d %H:%M:%S")
    log = logging.getLogger('maker')
    log.setLevel(logging.INFO)

def maker_libs_directory(): 
    return os.path.join(os.environ['HOME'], ".maker", "maker-libs", MAKER_VERSION)

def maker_scala_directory():  
    return os.path.join(os.environ['HOME'], ".maker", "scala-libs", MAKER_SCALA_VERSION)

def maker_resource_cache():
    return os.path.join(os.environ['HOME'], ".maker", "resource-cache")



def mkdir_p(directory):
    if not os.path.isdir(directory):
        os.makedirs(directory)
        log.info("Created " + directory)

def rm_rf(directory):
    shutil.rmtree(directory, ignore_errors=True)
    log.info("Deleted " + directory)


def create_maker_lib_directories():
    if args.refresh:
        rm_rf(maker_libs_directory())
        rm_rf(maker_scala_directory())
    mkdir_p(maker_libs_directory())
    mkdir_p(maker_scala_directory())


class Resource(object):
    def __init__(self, resolver, org, artifact, version):
        self.basename = artifact + "-" + version + ".jar"
        self.resolver = resolver
        relative_file = os.path.join(org.replace(".", "/"), artifact, version, self.basename)
        self.cache_file = os.path.join(maker_resource_cache(), relative_file)
        self.relative_url = pathname2url(relative_file) 

        mkdir_p(os.path.dirname(self.cache_file))

    def download_to(self, filename):
        url = urljoin(self.resolver, self.relative_url)
        log.info(url)
        log.info("\tDownloading")
        try:
            req = Request(url, None, {'Pragma': 'no-cache'})
            f = urlopen(url)

            # Open our local file for writing
            with open(filename, "wb") as local_file:
                local_file.write(f.read())
            log.info("\tDownloaded")

        except HTTPError, e:
            log.critical("HTTP Error: %s %s", e.code, url)
            exit(1)
        except URLError, e:
            log.critical("URL Error: %s %s", e.reason, url)
            exit(1)




def download_required_dependencies(resources, lib_dir):

    log.info("Downloading maker dependencies")
    temp_dir= tempfile.mkdtemp()

    for (resolver, org, artifact, version) in resources:
        resource = Resource(resolver, org, artifact, version)
        lib_file = os.path.join(lib_dir, resource.basename)
        if not os.path.isfile(lib_file):
            temp_file = os.path.join(temp_dir, resource.basename)
            resource.download_to(temp_file)
            shutil.copy(temp_file, lib_file)
            shutil.move(temp_file, resource.cache_file)

    rm_rf(temp_dir)

def recompile_project_source():
    if not args.project_src_dir:
        return


def launch_repl():
    mkdir_p(".maker")
    output_file = open(os.path.join(".maker", "repl_output"), "a")
    
    repl_classpath = ':'.join(glob(maker_scala_directory() + "/*.jar"))
    maker_classpath = ':'.join(glob(maker_libs_directory() + "/*.jar"))
    print(maker_classpath)

    call([  os.path.join(os.environ['JAVA_HOME'], "bin", "java"),
            "-classpath", repl_classpath,
            "-Dsbt.log.format=false",
            "-Dscala.usejavacp=true",
            "scala.tools.nsc.MainGenericRunner",
            "-cp", maker_classpath,
            "-Yrepl-sync", 
            "-nc"])
#    ,
#            "-i",
#            "Maker.scala"])





read_args()
print(args)
if args.project_src_dir:
    print(args.project_src_dir)
else:
    print("No project source")
import sys
exit(0)
create_logger()
create_maker_lib_directories()
download_required_dependencies(SCALA_RESOURCES, maker_scala_directory())
download_required_dependencies(MAKER_RESOURCES, maker_libs_directory())
launch_repl()


