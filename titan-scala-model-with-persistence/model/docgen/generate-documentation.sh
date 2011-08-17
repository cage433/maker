#!/bin/sh

cd $(dirname $0)

ruby ../../../mdl/bindinggen.rb \
        -o target \
        -b documentation-bindings.rb \
        ../master-model.rb
