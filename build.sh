#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

nim \
    r \
    --filenames=canonical \
    --hints=on \
    --hint=all:off \
    tests/parse_test.nim
# tests/lex_test.nim

# src/haxorg/parse/parse_org_old.nim
# src/haxorg/parse/lex_structure_test.nim "lex commands::*"

# nim \
#     check \
#     --filenames=canonical \
#     --hints=on \
#     --warning=all:off \
#     --hint=all:off \
#     tests/lex_test.nim

echo ">>> end"
# --hint=processing:on \
# --processing=filenames \
