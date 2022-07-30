#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

nim \
    r \
    --filenames=canonical \
    --hints=on \
    --hint=all:off \
    src/haxorg/parse/lex_all.nim
echo ">>> end"
# --hint=processing:on \
# --processing=filenames \
