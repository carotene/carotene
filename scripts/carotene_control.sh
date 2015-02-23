#!/bin/sh -e

exec erl \
    -pa ebin/ deps/**/ebin/ \
    -noinput \
    -hidden \
    -sname control_carotene \
    -s carotene_control \
    -extra "$@" \
