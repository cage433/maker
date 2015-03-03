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
MAKER_VERSION       = "1.9-SNAPSHOT"
MAKER_SCALA_VERSION = "2.10.4"
SONATYPE            = "http://oss.sonatype.org/content/repositories/snapshots/"
MAVEN               = "http://repo1.maven.org/maven2/"
TYPESAFE            = "http://repo.typesafe.com/typesafe/releases/"
MAKER_BINARIES      = [ (SONATYPE, "com.github.cage433", "maker", MAKER_VERSION),
                        (SONATYPE, "com.github.cage433", "maker-test-reporter", MAKER_VERSION)
                        ]
SCALA_LIBRARIES     = [ (TYPESAFE, "org.scala-lang", "scala-library", MAKER_SCALA_VERSION),
                        (TYPESAFE, "org.scala-lang", "jline", MAKER_SCALA_VERSION),
                        (TYPESAFE, "org.scala-lang", "scala-compiler", MAKER_SCALA_VERSION),
                        (TYPESAFE, "org.scala-lang", "scala-reflect", MAKER_SCALA_VERSION)]
# GENERATED MAKER DEPENDENCIES
MAKER_DEPENDENCIES  = [
	(MAVEN, org/scalatest, scalatest_2.10, 2.2.0)
	(MAVEN, org/scala-lang, scala-library, 2.10.4)
	(MAVEN, org/scala-lang, scala-reflect, 2.10.4)
	(MAVEN, ch/qos/logback, logback-classic, 1.0.6)
	(MAVEN, ch/qos/logback, logback-core, 1.0.6)
	(MAVEN, org/slf4j, slf4j-api, 1.6.5)
	(MAVEN, org/slf4j, jcl-over-slf4j, 1.6.1)
	(MAVEN, commons-io, commons-io, 2.1)
	(MAVEN, com/typesafe/zinc, zinc, 0.3.5)
	(MAVEN, com/typesafe/sbt, incremental-compiler, 0.13.5)
	(MAVEN, org/scala-lang, scala-compiler, 2.10.3)
	(MAVEN, com/typesafe/sbt, sbt-interface, 0.13.5)
	(MAVEN, com/typesafe/sbt, compiler-interface, 0.13.5)
	(MAVEN, org/apache/httpcomponents, httpclient, 4.3)
	(MAVEN, org/apache/httpcomponents, httpcore, 4.3)
	(MAVEN, commons-logging, commons-logging, 1.1.3)
	(MAVEN, commons-codec, commons-codec, 1.6)
	(MAVEN, org/apache/ivy, ivy, 2.3.0-rc2)
	(MAVEN, org/scalaz, scalaz-core_2.10, 7.0.1)
	(MAVEN, com/google/guava, guava, 11.0.2)
	(MAVEN, com/google/code/findbugs, jsr305, 1.3.9)
	(MAVEN, com/typesafe, config, 1.2.1)
	(MAVEN, io/spray, spray-json_2.10, 1.3.1)
	(MAVEN, javax/inject, javax.inject, 1)
	(MAVEN, org/apache/maven, maven-aether-provider, 3.2.5)
	(MAVEN, org/apache/maven, maven-model, 3.2.5)
	(MAVEN, org/apache/maven, maven-model-builder, 3.2.5)
	(MAVEN, org/codehaus/plexus, plexus-interpolation, 1.21)
	(MAVEN, org/apache/maven, maven-repository-metadata, 3.2.5)
	(MAVEN, org/eclipse/aether, aether-api, 1.0.0.v20140518)
	(MAVEN, org/eclipse/aether, aether-spi, 1.0.0.v20140518)
	(MAVEN, org/eclipse/aether, aether-util, 1.0.0.v20140518)
	(MAVEN, org/codehaus/plexus, plexus-component-annotations, 1.5.5)
	(MAVEN, org/codehaus/plexus, plexus-utils, 3.0.20)
	(MAVEN, org/eclipse/aether, aether-connector-basic, 1.0.0.v20140518)
	(MAVEN, org/eclipse/aether, aether-impl, 1.0.0.v20140518)
	(MAVEN, org/eclipse/aether, aether-transport-file, 1.0.0.v20140518)
	(MAVEN, org/eclipse/aether, aether-transport-http, 1.0.0.v20140518)
	(MAVEN, org/mortbay/jetty, jetty, 6.1.26)
	(MAVEN, org/mortbay/jetty, jetty-util, 6.1.26)
	(MAVEN, org/mortbay/jetty, servlet-api, 2.5-20081211)
]
# GENERATED MAKER DEPENDENCIES END


def maker_directory():
    return os.path.dirname(os.path.abspath(__file__))

def read_args():
    global args
    parser = argparse.ArgumentParser()

    parser.add_argument('-r', '--refresh', action='store_true', dest='refresh', default=False)
    parser.add_argument('-c', '--project-source-dir', dest='project_src_dir')
    parser.add_argument('-p', '--project-definition-file', dest='project_definition_file')
    parser.add_argument('-l', '--logback-config', dest='logback_config', default=os.path.join('logback-config', 'logback.xml'))
    parser.add_argument('-z', '--maker-developer-mode', dest='maker_developer_mode', action='store_true', default = False)
    parser.add_argument('-j', '--use-jrebel', dest='use_jrebel', action='store_true', default = False)
    parser.add_argument('-J', '--JVM-ARGS', dest='jvm_args', nargs=argparse.REMAINDER, default = [])
    parser.add_argument('-e', '--extra-classpath', dest='extra_classpath', help='Colon separated list of directories/jars')
    args = parser.parse_args()

    if args.refresh and not args.project_src_dir:
        log.fatal("'refresh' called with no project src directory")
        exit(1)

def create_logger():
    global log
    logging.basicConfig( \
            format= "%(asctime)-15s %(levelname)-10s %(message)s", \
            datefmt="%Y-%m-%d %H:%M:%S")
    log = logging.getLogger('maker')
    log.setLevel(logging.INFO)

def maker_dependencies_directory(): 
    return os.path.join(os.environ['HOME'], ".maker", "maker-dependencies", MAKER_VERSION)

def maker_binaries_directory(): 
    return os.path.join(os.environ['HOME'], ".maker", "maker-binaries", MAKER_VERSION)

def maker_scala_directory():  
    return os.path.join(os.environ['HOME'], ".maker", "scala-libs", MAKER_SCALA_VERSION)

def maker_resource_cache():
    return os.path.join(os.environ['HOME'], ".maker", "resource-cache")

def project_class_directory():
    return mkdir_p(os.path.join(".maker", "project-classes"))

def reference_config_directory():
    return os.path.join(maker_directory(), "config")

def project_definition_file():
    if args.project_definition_file:
        return args.project_definition_file

    scala_files_in_pwd = glob('*.scala')
    if len(scala_files_in_pwd) == 1:
        log.info("Using %s as project file", scala_files_in_pwd[0])
        return scala_files_in_pwd[0]
    else:
        log.critical("Maker requires a project file - exiting")
        exit(1)


def mkdir_p(directory):
    if not os.path.isdir(directory):
        os.makedirs(directory)
    return directory

def rm_rf(directory):
    shutil.rmtree(directory, ignore_errors=True)

def create_maker_lib_directories():
    if args.refresh:
        rm_rf(maker_dependencies_directory())
        rm_rf(maker_binaries_directory())
        rm_rf(maker_scala_directory())
    mkdir_p(maker_dependencies_directory())
    mkdir_p(maker_binaries_directory())
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

    temp_dir= tempfile.mkdtemp()

    for (resolver, org, artifact, version) in resources:
        resource = Resource(resolver, org, artifact, version)
        lib_file = os.path.join(lib_dir, resource.basename)
        if not os.path.isfile(lib_file):
            if os.path.isfile(resource.cache_file):
                shutil.copy(resource.cache_file, lib_file)
            else:
                temp_file = os.path.join(temp_dir, resource.basename)
                resource.download_to(temp_file)
                shutil.copy(temp_file, lib_file)
                shutil.move(temp_file, resource.cache_file)

    rm_rf(temp_dir)

def project_src_needs_recompiling():
    if args.refresh:
        return True

    if not args.project_src_dir:
        return False

    src_files = glob(args.project_src_dir + "/*.scala")
    if len(src_files) == 0:
        return False

    class_files = os.listdir(project_class_directory())
    if len(class_files) == 0:
        return True

    return max(src_files, key=os.path.getctime) > min(class_files, key=os.path.getctime)


def java():
    return os.path.join(os.environ['JAVA_HOME'], "bin", "java")

def scala_libraries():
    return glob(maker_scala_directory() + "/*.jar")

def maker_dependencies():
    # TODO - drop test reporter from this
    return glob(maker_dependencies_directory() + "/*.jar")

def maker_binaries():
    return glob(maker_binaries_directory() + "/*.jar")

def classpath(jars_and_directories):
    return ':'.join(jars_and_directories)

def maker_class_directories():
    maker_root = os.path.dirname(os.path.realpath(__file__))
    return [os.path.join(maker_root, module, "target-maker", "classes") for module in ["utils", "maker"]]


def recompile_project_source():
    if not project_src_needs_recompiling():
        return

    log.info("Recompiling project source files")

    classpath_components = scala_libraries() + maker_dependencies()
    if args.maker_developer_mode:
        classpath_components.extend(maker_class_directories())
    else:
        classpath_components.extend(maker_binaries())

    result = call([ java(),
                    "-classpath", classpath(classpath_components),
                    "-Dscala.usejavacp=true",
                    "scala.tools.nsc.Main",
                    "-d", project_class_directory()] + 
                    glob(args.project_src_dir + "/*.scala"))
    if result != 0:
        log.critical("Failed to compile project source - exiting")
        exit(1)


def launch_repl():
    mkdir_p(".maker")
    
    classpath_components = scala_libraries() + maker_dependencies() + [project_class_directory(), reference_config_directory()]
    if args.maker_developer_mode:
        classpath_components.extend(maker_class_directories())
    else:
        classpath_components.extend(maker_binaries())

    if args.extra_classpath:
        classpath_components.extend(args.extra_classpath.split(":"))

    if args.use_jrebel:
        extra_opts = ["-javaagent:/usr/local/jrebel/jrebel.jar"]
    else:
        extra_opts = []


    cmd_args=[  java(),
            "-classpath", classpath(scala_libraries()),
            "-Dsbt.log.format=false",
            "-Drebel.log=true",
            "-Dscala.usejavacp=true"] + extra_opts + args.jvm_args + ["-Dlogback.configurationFile=" + args.logback_config,
            "scala.tools.nsc.MainGenericRunner",
            "-cp", classpath(classpath_components),
            "-Yrepl-sync", 
            "-nc", 
            "-i", project_definition_file()]

    call(cmd_args)



create_logger()
read_args()
create_maker_lib_directories()

log.info("Checking for missing resources")
download_required_dependencies(SCALA_LIBRARIES, maker_scala_directory())
download_required_dependencies(MAKER_DEPENDENCIES, maker_dependencies_directory())
download_required_dependencies(MAKER_BINARIES, maker_binaries_directory())

log.info("Checking for stale project class files")
recompile_project_source()

log.info("Launching repl")
launch_repl()


