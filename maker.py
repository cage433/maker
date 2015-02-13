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

# Computer generated section
MAKER_VERSION   = "0.1"
SONATYPE        = "http://oss.sonatype.org/content/repositories/releases/"
MAVEN           = "http://repo1.maven.org/maven2/"
TYPESAFE        = "http://repo.typesafe.com/typesafe/releases/"
RESOURCES       = [ (TYPESAFE, "org.scala-lang", "scala-library", "2.10.4"),
                    (TYPESAFE, "org.scala-lang", "jline", "2.10.4"),
                    (TYPESAFE, "org.scala-lang", "scala-compiler", "2.10.4"),
                    (TYPESAFE, "org.scala-lang", "scala-reflect", "2.10.4"),
                    (MAVEN, "org.scalatest", "scalatest_2.10", "2.2.0"),
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
                    (MAVEN, "io.spray", "spray-json_2.10", "1.3.1")]
# End computer generated section


def read_args():
    global args
    parser = argparse.ArgumentParser()

    parser.add_argument('-r', '--refresh-maker-dependencies', action='store_true', dest='refresh', default=False)
    args = parser.parse_args()

def create_logger():
    global log
    logging.basicConfig( \
            format= "%(asctime)-15s %(levelname)-10s Maker: %(message)s", \
            datefmt="%Y-%m-%d %H:%M:%S")
    log = logging.getLogger('maker')
    log.setLevel(logging.INFO)

maker_libs_directory = os.path.join(os.environ['HOME'], ".maker", MAKER_VERSION, "maker-libs")
maker_resource_cache = os.path.join(os.environ['HOME'], ".maker", "resource-cache")

def mkdir_p(directory):
    if not os.path.isdir(directory):
        os.makedirs(directory)
        log.info("Created " + directory)

def rm_rf(directory):
    shutil.rmtree(directory, ignore_errors=True)
    log.info("Deleted " + directory)

def create_maker_libs_directory():
    if args.refresh:
        rm_rf(maker_libs_directory)
    mkdir_p(maker_libs_directory)

class Resource(object):
    def __init__(self, resolver, org, artifact, version):
        self.basename = artifact + "-" + version + ".jar"
        self.resolver = resolver
        relative_file = os.path.join(org.replace(".", "/"), artifact, version, self.basename)
        self.cache_file = os.path.join(maker_resource_cache, relative_file)
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




def download_required_dependencies():

    if not args.refresh and os.path.isdir(maker_libs_directory):
        log.info("All dependencies downloaded")
        return 

    create_maker_libs_directory()
    log.info("Downloading maker dependencies")
    temp_dir= tempfile.mkdtemp()

    for (resolver, org, artifact, version) in RESOURCES:
        resource = Resource(resolver, org, artifact, version)
        temp_file = os.path.join(temp_dir, resource.basename)
        resource.download_to(temp_file)
        shutil.copy(temp_file, maker_libs_directory)
        shutil.move(temp_file, resource.cache_file)



    

read_args()
create_logger()
download_required_dependencies()




