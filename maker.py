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
import sys

# Computer generated section
MAKER_VERSION       = "0.45"
MAKER_SCALA_VERSION = "2.10.4"
SONATYPE_SNAPSHOTS  = "http://oss.sonatype.org/content/repositories/snapshots/"
SONATYPE_RELEASES   = "http://oss.sonatype.org/content/repositories/releases/"
MAVEN               = "http://repo1.maven.org/maven2/"
TYPESAFE            = "http://repo.typesafe.com/typesafe/releases/"
MAKER_BINARIES      = [ (SONATYPE_RELEASES, "com.github.cage433", "maker_2.10", MAKER_VERSION) ]
SCALA_LIBRARIES     = [ (MAVEN, "org.scala-lang", "scala-library", MAKER_SCALA_VERSION),
                        (MAVEN, "org.scala-lang", "jline", MAKER_SCALA_VERSION),
                        (MAVEN, "org.scala-lang", "scala-compiler", MAKER_SCALA_VERSION),
                        (MAVEN, "org.scala-lang", "scala-reflect", MAKER_SCALA_VERSION)]
# GENERATED MAKER DEPENDENCIES
MAKER_DEPENDENCIES  = [
	(MAVEN, "org/scala-lang", "scala-library", "2.10.4"),
	(MAVEN, "org/scala-lang", "scala-compiler", "2.10.4"),
	(MAVEN, "org/scala-lang", "scala-reflect", "2.10.4"),
	(MAVEN, "org/scalatest", "scalatest_2.10", "3.0.1"),
	(MAVEN, "org/scalactic", "scalactic_2.10", "3.0.1"),
	(MAVEN, "ch/qos/logback", "logback-classic", "1.2.1"),
	(MAVEN, "ch/qos/logback", "logback-core", "1.2.1"),
	(MAVEN, "org/slf4j", "slf4j-api", "1.7.22"),
	(MAVEN, "org/slf4j", "jcl-over-slf4j", "1.7.24"),
	(MAVEN, "commons-io", "commons-io", "2.5"),
	(MAVEN, "com/typesafe/zinc", "zinc", "0.3.13"),
	(MAVEN, "com/typesafe/sbt", "incremental-compiler", "0.13.13"),
	(MAVEN, "com/typesafe/sbt", "sbt-interface", "0.13.13"),
	(MAVEN, "org/apache/httpcomponents", "httpclient", "4.5.3"),
	(MAVEN, "org/apache/httpcomponents", "httpcore", "4.4.6"),
	(MAVEN, "commons-logging", "commons-logging", "1.2"),
	(MAVEN, "commons-codec", "commons-codec", "1.9"),
	(MAVEN, "org/scalaz", "scalaz-core_2.10", "7.2.9"),
	(MAVEN, "io/spray", "spray-json_2.10", "1.3.3"),
	(MAVEN, "javax/inject", "javax.inject", "1"),
	(MAVEN, "org/apache/commons", "commons-exec", "1.3"),
	(MAVEN, "org/apache/maven", "maven-aether-provider", "3.3.9"),
	(MAVEN, "org/apache/maven", "maven-model", "3.3.9"),
	(MAVEN, "org/apache/maven", "maven-model-builder", "3.3.9"),
	(MAVEN, "org/codehaus/plexus", "plexus-interpolation", "1.21"),
	(MAVEN, "org/apache/maven", "maven-artifact", "3.3.9"),
	(MAVEN, "org/apache/maven", "maven-builder-support", "3.3.9"),
	(MAVEN, "org/apache/maven", "maven-repository-metadata", "3.3.9"),
	(MAVEN, "org/eclipse/aether", "aether-api", "1.0.2.v20150114"),
	(MAVEN, "org/eclipse/aether", "aether-spi", "1.0.2.v20150114"),
	(MAVEN, "org/eclipse/aether", "aether-util", "1.0.2.v20150114"),
	(MAVEN, "org/codehaus/plexus", "plexus-component-annotations", "1.6"),
	(MAVEN, "org/codehaus/plexus", "plexus-utils", "3.0.22"),
	(MAVEN, "org/apache/commons", "commons-lang3", "3.4"),
	(MAVEN, "org/eclipse/aether", "aether-connector-basic", "1.1.0"),
	(MAVEN, "org/eclipse/aether", "aether-impl", "1.1.0"),
	(MAVEN, "org/eclipse/aether", "aether-transport-file", "1.1.0"),
	(MAVEN, "org/eclipse/aether", "aether-transport-http", "1.1.0"),
	(MAVEN, "org/eclipse/aether", "aether-test-util", "1.1.0"),
	(MAVEN, "com/google/guava", "guava", "21.0"),
	(MAVEN, "com/github/cage433", "maker-test-reporter_2.10", "0.45")
]
# GENERATED MAKER DEPENDENCIES END


def maker_directory():
    return os.path.dirname(os.path.abspath(__file__))

def print_and_flush(msg):
  sys.stdout.write(msg)
  sys.stdout.flush()

def read_args():
    global args
    parser = argparse.ArgumentParser()

    parser.add_argument('-r', '--refresh', action='store_true', dest='refresh', default=False)
    parser.add_argument('-p', '--project-definition-file', dest='project_definition_file')
    parser.add_argument('-l', '--logback-config', dest='logback_file')
    parser.add_argument('-L', '--python_log_level', dest='python_log_level', default=logging.INFO)
    parser.add_argument('-z', '--maker-developer-mode', dest='maker_developer_mode', action='store_true', default = False)
    parser.add_argument('-j', '--use-jrebel', dest='use_jrebel', action='store_true', default = False)
    parser.add_argument('-J', '--JVM-ARGS', dest='jvm_args', nargs=argparse.REMAINDER, default = [])
    parser.add_argument('-e', '--extra-classpath', dest='extra_classpath', help='Colon separated list of directories/jars')
    parser.add_argument('-E', '--execute-command', dest='execute_command', help='maker command to run (and then exit)')
    (args, unknown_args) = parser.parse_known_args()
    if unknown_args:
        print_and_flush("unknown args" + " - ".join(unknown_args))
    args


def ask_user(question, default):
  sys.stdout.write(question + ": ")
  sys.stdout.flush()
  return raw_input("") or default

def confirm(question):
  return (ask_user(question, "Y")).upper() == "Y"

def logback_file():
  if args.logback_file:
    return os.path.abspath(args.logback_file)
  else: 
    logback_file = os.path.abspath("logback.xml")
    if os.path.isfile("logback.xml"):
      log.info("No logback file specified, using " + logback_file)
      return logback_file
    elif confirm("Create logback config file " + logback_file):
      f = open(logback_file, 'w+')
      for line in [
          "<!--The logback file used by the maker repl",
          "    In normal use maker shouldn't log much, if at all, however",
          "    logging isn't switched off in case of messages from other libraries-->",
          "",
          "<configuration scan=\"true\" scanPeriod=\"3 seconds\">",
          "",
          "  <appender name=\"FILE\" class=\"ch.qos.logback.core.FileAppender\">",
          "    <file>maker.log</file>",
          "    <append>true</append>",
          "    <encoder>",
          "      <pattern>%d{HH:mm:ss.SSS} [%thread] %-5level - %msg%n</pattern>",
          "      <immediateFlush>true</immediateFlush>",
          "    </encoder>",
          "    <filter class=\"ch.qos.logback.classic.filter.ThresholdFilter\">",
          "      <level>INFO</level>",
          "    </filter>",
          "  </appender>",
          "        ",
          "  <appender name=\"CONSOLE\" class=\"ch.qos.logback.core.ConsoleAppender\">",
          "    <encoder>",
          "      <pattern>%d{HH:mm:ss.SSS} [%thread] %-5level - %msg%n</pattern>",
          "      <immediateFlush>true</immediateFlush>",
          "    </encoder>",
          "    <filter class=\"ch.qos.logback.classic.filter.ThresholdFilter\">",
          "      <level>WARN</level>",
          "    </filter>",
          "  </appender>",
          "",
          "  <root level=\"info\">",
          "    <appender-ref ref=\"CONSOLE\" />",
          "    <appender-ref ref=\"FILE\" />",
          "  </root>",
          " </configuration>"
          ]:
        f.write("%s\n" % line)
      f.close
      return logback_file
    else:
      log.warn("Refusing to run without a logback file - exiting")
      sys.exit(1)



def create_logger():
    global log
    logging.basicConfig( \
            format= "%(asctime)-15s %(levelname)-10s %(message)s", \
            datefmt="%Y-%m-%d %H:%M:%S")
    log = logging.getLogger('maker')
    log.setLevel(int(args.python_log_level))

def maker_dependencies_directory(): 
    return os.path.join(os.environ['HOME'], ".maker", "maker-dependencies", MAKER_VERSION)

def maker_binaries_directory(): 
    return os.path.join(os.environ['HOME'], ".maker", "maker-binaries", MAKER_VERSION)

def maker_scala_directory():  
    return os.path.join(os.environ['HOME'], ".maker", "scala-libs", MAKER_SCALA_VERSION)

def maker_resource_cache():
    return os.path.join(os.environ['HOME'], ".maker", "resource-cache")


def project_definition_file():
    if args.project_definition_file:
        return args.project_definition_file
    elif os.path.isfile(os.path.join("maker", "Project.scala")):
        log.debug("Using maker/Project.scala as project definition file")
    elif confirm("Create minimal project [Y/n]"):
        name = ask_user("Project name", None)
        mkdir_p(os.path.join(name, "src", "main", "scala", "org", name))

        src_file = os.path.join(name, "src", "main", "scala", "org", name, "Foo.scala")
        f = open(src_file, 'w+')
        f.write("package org." + name + "\n")
        f.write("\n")
        f.write("case class Foo(n: Int)\n")
        f.close

        mkdir_p(os.path.join(name, "src", "test", "scala", "org", name))
        test_file = os.path.join(name, "src", "test", "scala", "org", name, "FooTest.scala")
        f = open(test_file, 'w+')
        f.write("package org." + name + "\n")
        f.write("\n")
        f.write("import org.scalatest.FunSuite\n")
        f.write("import org.scalatest.Matchers\n")
        f.write("\n")
        f.write("class FooTest extends FunSuite with Matchers {\n")
        f.write("\n")
        f.write("\ttest(\"Dummy test\") {\n")
        f.write("\t\tFoo(17).n should be (17)\n")
        f.write("\t}\n")
        f.write("}\n")
        f.close

        mkdir_p("maker")
        project_file = os.path.join("maker", "Project.scala")
        f = open(project_file, 'w+')
        f.write("import maker.project.DependencyPimps._\n")
        f.write("import maker.project.Module\n\n")
        f.write("lazy val " + name + "Module = new Module(\n")
        f.write("\troot = new java.io.File(\"" + name + "\"),\n")
        f.write("\tname = \"" + name + "\"\n")
        f.write(") {\n")
        f.write("\toverride def dependencies() = Seq(\"org.scalatest\" % \"scalatest\" %%  \"2.2.0\")\n")
        f.write("}\n\n")
        f.write("import " + name + "Module._\n")

        f.close

    else:
        log.critical("Maker requires a project file - exiting")
        sys.exit(1)

      

    scala_files_in_pwd = glob('maker/Project.scala')
    if len(scala_files_in_pwd) == 1:
        log.info("Using %s as project file", scala_files_in_pwd[0])
        return scala_files_in_pwd[0]
    else:
        log.critical("Maker requires a project file - exiting")
        sys.exit(1)


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
            sys.exit(1)
        except URLError, e:
            log.critical("URL Error: %s %s", e.reason, url)
            sys.exit(1)




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


def java():
    return os.path.join(os.environ['JAVA_HOME'], "bin", "java")

def maker_scala_libraries():
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
    return [os.path.join(maker_root, module, "target-maker", MAKER_SCALA_VERSION, "classes") for module in ["maker"]]

def maker_test_class_directories():
    maker_root = os.path.dirname(os.path.realpath(__file__))
    return [os.path.join(maker_root, module, "target-maker", MAKER_SCALA_VERSION, "test-classes") for module in ["maker"]]

def test_reporter_classpath_component():
    if args.maker_developer_mode:
        maker_root = os.path.dirname(os.path.realpath(__file__))
        return os.path.join(maker_root, "test-reporter", "target-maker", MAKER_SCALA_VERSION, "classes") 
    else:
        for jar in maker_dependencies():
            if "test-reporter" in jar:
                return jar
    log.critical("Can't find test reporter jar")
    sys.exit(1)


def launch_repl():
    mkdir_p(".maker")
    
    classpath_components = maker_scala_libraries() + maker_dependencies() 

    if args.maker_developer_mode:
        classpath_components.extend(maker_class_directories())
        classpath_components.extend(maker_test_class_directories())
    else:
        classpath_components.extend(maker_binaries())

    if args.extra_classpath:
        classpath_components.extend(args.extra_classpath.split(':'))

    if args.use_jrebel:
        extra_opts = ["-javaagent:/usr/local/jrebel/jrebel.jar"]
    else:
        extra_opts = []

    if args.execute_command:
        extra_opts = extra_opts + ["-Dmaker.exec-mode=true"]

    cmd_args=[  java(),
            "-classpath", classpath(maker_scala_libraries()),
            "-Dsbt.log.format=false",
            "-Drebel.log=true",
            "-Dmaker.version=" + MAKER_VERSION,
            "-Dmaker.test.reporter=" + test_reporter_classpath_component(),
            "-Dscala.usejavacp=true"] + extra_opts + args.jvm_args + \
                ["-Dlogback.configurationFile=" + logback_file(),
            "scala.tools.nsc.MainGenericRunner",
            "-cp", classpath(classpath_components),
            "-Yrepl-sync", 
            "-nc", 
            "-i", project_definition_file()]

    if args.execute_command:
        cmd_args = cmd_args + ["-e"] + args.execute_command.split() 

    return call(cmd_args)



read_args()
create_logger()
create_maker_lib_directories()


download_required_dependencies(SCALA_LIBRARIES, maker_scala_directory())
download_required_dependencies(MAKER_DEPENDENCIES, maker_dependencies_directory())
download_required_dependencies(MAKER_BINARIES, maker_binaries_directory())


return_code = launch_repl()
sys.exit(return_code)


