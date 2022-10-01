#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o errexit

function test() {
    echo ">> " $1
    nim \
        r \
        --filenames=canonical \
        --hints=on \
        --hint=all:off \
        $1 \
        "$2"
}

test tests/lex_test.nim
test tests/parse_test.nim # "Text parsing::Lists"
# test tests/parse_test.nim "Text parsing::Lists"

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
