#!/usr/bin/python

# Load scala 2.10 into ~/.maker
# load zinc into ~/.maker
# Launch zinc on a unique port

# Download of scala version should be a maker task
import re, shutil, sys
from os import path, getenv, makedirs
from urllib import pathname2url
from urlparse import urljoin
from urllib2 import urlopen, URLError, HTTPError, Request

class MakerResourceConfig(object):
    def __init__(self):
        self.maker_root_directory = path.dirname(path.dirname(path.realpath(__file__)))
        self.maker_resource_config = path.join(self.maker_root_directory, "external-resource-config")

        # Collect versions and resolvers
        self.resource_versions = {}
        resource_resolvers = {}

        with open(self.maker_resource_config) as f:
            content = f.readlines()
            for line in content:
                version_match = re.match(r'^version:\s+(\w+)\s+(\w+)', line)
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
        self.artifact = artifact
        self.version = version
        self.config = MakerResourceConfig()
        self.relative_dir = path.join(org.replace(".", "/"), artifact, version)
        self.basename = artifact + "-" + version + ".jar"
        self.relative_file = path.join(self.relative_dir, self.basename)
        self.relative_url = pathname2url(self.relative_file)
        self.cache_file = path.join(getenv("HOME"), ".maker/cache/", self.relative_file)
        self.cache_dir = path.dirname(self.cache_file)
        if not path.isdir(self.cache_dir):
            makedirs(self.cache_dir)

    def try_to_download_from(self, resolver):
        url = urljoin(resolver, self.relative_url)
        print "Trying to download " + url
        try:
            req = Request(url, None, {'Pragma': 'no-cache'})
            f = urlopen(url)

            # Open our local file for writing
            with open(path.basename(url), "wb") as local_file:
                local_file.write(f.read())
            print "Downloaded " + url
            return True

        #handle errors
        except HTTPError, e:
            print "HTTP Error:", e.code, url
            return False
        except URLError, e:
            print "URL Error:", e.reason, url
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
            print "Not able to download " + self.relative_url
            sys.exit(1)


config = MakerResourceConfig()

for artifact in ["scala-library", "scala-reflect", "jline"]:
    MakerResource("org.scala-lang", artifact, "2.10.4").download_to(path.join(config.maker_root_directory, "scala-libs"))


