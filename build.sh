#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

nim \
    r \
    --filenames=canonical \
    --hints=on \
    --hint=all:off \
    src/haxorg/parse/lex_text_test.nim "Lex embedded structures::link protocols"
nim \
    check \
    --filenames=canonical \
    --hints=on \
    --warning=all:off \
    --hint=all:off \
    src/haxorg/parse/lex_test.nim

echo ">>> end"
# --hint=processing:on \
# --processing=filenames \
