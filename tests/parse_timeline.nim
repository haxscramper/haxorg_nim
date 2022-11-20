import hmisc/preludes/unittest
import hmisc/core/all
import haxorg/[types, parser]
import std/strutils


let tree = orgParse(readFile("/mnt/workspace/repos/fic/wiki/timeline.org"))
# let tree = orgParse(readFile("/tmp/timeline.org"))
