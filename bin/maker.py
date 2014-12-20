#!/usr/bin/python

# Load scala 2.10 into ~/.maker
# load zinc into ~/.maker
# Launch zinc on a unique port

# Download of scala version should be a maker task
import re, shutil, sys, logging
from os import path, getenv, makedirs
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


class MakerResource(object):
    def __init__(self, org, artifact, version):
        self.org = org
        self.config = MakerResourceConfig()
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
maker_lib_dir = path.join(config.maker_root_directory, "maker-libs")

for artifact in ["scala-library", "scala-reflect", "jline"]:
    MakerResource("org.scala-lang", artifact, config.resource_versions["scala_version"]).download_to(maker_lib_dir)

for artifact in ["aether-api", "aether-util", "aether-impl", "aether-connector-basic", "aether-transport-file", "aether-transport-http"]:
    MakerResource("org.eclipse.aether", artifact, config.resource_versions["aether_version"]).download_to(maker_lib_dir)

