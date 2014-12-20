#!/usr/bin/python

# Load scala 2.10 into ~/.maker
# load zinc into ~/.maker
# Launch zinc on a unique port

# Download of scala version should be a maker task
import re, shutil, sys, logging, tempfile, subprocess
import fnmatch
from glob import glob
from os import path, getenv, makedirs, walk
from urllib import pathname2url
from urlparse import urljoin
from urllib2 import urlopen, URLError, HTTPError, Request

logging.basicConfig( \
        format= "%(asctime)-15s %(levelname)-10s Maker:%(message)s", \
        datefmt="%Y-%m-%d %H:%M:%S")
log = logging.getLogger('maker')
log.setLevel(logging.INFO)

class MakerResourceConfig(object):
    def __init__(self):
        self.maker_root_directory = path.dirname(path.dirname(path.realpath(__file__)))

        # Collect versions and resolvers
        self.resource_versions = {}
        resource_resolvers = {}

        maker_resource_config = path.join(self.maker_root_directory, "external-resource-config")
        with open(maker_resource_config) as f:
            content = f.readlines()
            for line in content:
                version_match = re.match(r'^version:\s+(\w+)\s+([0-9a-zA-Z.]+)', line)
                if version_match:
                    self.resource_versions[version_match.group(1)] = version_match.group(2)

                resolver_match = re.match(r'^resolver:\s+(\w+)\s+([A-Za-z0-9:/.]+)', line)
                if resolver_match:
                    resource_resolvers[resolver_match.group(1)] = resolver_match.group(2)


        # Order resolvers, putting `default` first if
        # it exists
        self.resolver_urls = [url for (name, url) in resource_resolvers.viewitems() if name != "default"]
        if resource_resolvers.has_key("default"):
            self.resolver_urls.insert(0, resource_resolvers["default"])
    def replace_version(self, string):
        version_match = re.match(r'([^{]*)\{([^}]+)}', string)
        if version_match:
            return version_match.group(1) + self.resource_versions[version_match.group(2)]
        else:
            return string




class MakerResource(object):
    def __init__(self, org, artifact, version):
        self.org = org
        self.config = MakerResourceConfig()
        artifact = config.replace_version(artifact)
        version = config.replace_version(version)
        self.basename = artifact + "-" + version + ".jar"
        relative_file = path.join(org.replace(".", "/"), artifact, version, self.basename)
        self.relative_url = pathname2url(relative_file) 
        self.cache_file = path.join(getenv("HOME"), ".maker/cache/", relative_file)
        
        if not path.isdir(path.dirname(self.cache_file)):
            makedirs(path.dirname(self.cache_file))

    def try_to_download_from(self, resolver):
        url = urljoin(resolver, self.relative_url)
        log.info("Trying to download %s", url)
        try:
            req = Request(url, None, {'Pragma': 'no-cache'})
            f = urlopen(url)

            # Open our local file for writing
            with open(path.basename(url), "wb") as local_file:
                local_file.write(f.read())
            log.info("Downloaded %s", url)
            return True

        except HTTPError, e:
            log.info("HTTP Error: %s %s", e.code, url)
            return False
        except URLError, e:
            log.info("URL Error: %s %s", e.reason, url)
            return False

    def download_to(self, directory):
        if not path.isdir(directory):
            makedirs(directory)

        download_file = path.join(directory, self.basename)

        if path.isfile(download_file):
            return 

        if path.isfile(self.cache_file):
            shutil.copy(self.cache_file, download_file)
            return 

        for resolver_url in config.resolver_urls:
            if self.try_to_download_from(resolver_url):
                break

        if path.isfile(self.basename):
            shutil.move(self.basename, self.cache_file)
            shutil.copy(self.cache_file, download_file)
        else:
            log.critical("Not able to download %s", self.relative_url)
            sys.exit(1)


config = MakerResourceConfig()
scala_lib_dir = path.join(config.maker_root_directory, "scala-libs")
for artifact in ["scala-library", "scala-reflect", "jline"]:
    MakerResource("org.scala-lang", artifact, config.resource_versions["scala_version"]).download_to(scala_lib_dir)

scala_compiler_dir = "scala-compiler-lib"
MakerResource("org.scala-lang", "scala-compiler", config.resource_versions["scala_version"]).download_to(scala_compiler_dir)

for module in ["test-reporter", "utils"]:
    external_resource_file = path.join(config.maker_root_directory, module, "external-resources")
    lib_dir = path.join(config.maker_root_directory, module, "lib_managed")
    with open(external_resource_file) as f:
        for line in f.readlines():
            resource_match = re.match(r'(\S+)\s+(\S+)\s+(\S+)', line)
            if resource_match:
                org, artifact, version = resource_match.groups()
                MakerResource(org, artifact, version).download_to(lib_dir)

def build_jar(jar_file, library_dirs, source_file_dirs):
    tmp_dir = tempfile.mkdtemp()
    class_path_jars = []
    for lib_dir in library_dirs + [scala_lib_dir]:
        class_path_jars += glob(lib_dir + "/*.jar")
    classpath = ":".join(class_path_jars)

    source_files = []
    for source_dir in source_file_dirs:
        for root, dirnames, filenames in walk(source_dir):
            for filename in fnmatch.filter(filenames, '*.scala'):
                source_files.append(path.join(root, filename))

    result = subprocess.call([\
            "java", \
            "-classpath", classpath, \
            "-Dscala.usejavacp=true", \
            "scala.tools.nsc.Main", \
            "-d", tmp_dir] + source_files)
    if result != 0:
        log.critical("Compilation failed when building %s", jar_file)
        sys.exit(2)

    jar_binary = path.join(getenv("JAVA_HOME"), "bin", "jar")
    result = subprocess.call([jar_binary, "cf", jar_file, "-C", tmp_dir, "."])
    if result != 0:
        log.critical("Failed to build jar ", jar_file)
        sys.exit(3)
    shutil.rmtree(tmp_dir)


build_jar("maker-scalatest-reporter.jar", ["test-reporter/lib_managed", scala_lib_dir, scala_compiler_dir], ["test-reporter/src"])
build_jar("maker.jar", ["test-reporter/lib_managed", "utils/lib_managed", scala_lib_dir, scala_compiler_dir], ["utils/src", "maker/src"])

