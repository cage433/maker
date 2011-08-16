#!/bin/bash

find .ivy -name '*.xml' -exec git update-index --assume-unchanged {} ";"
find .ivy -name '*.properties' -exec git update-index --assume-unchanged {} ";"
