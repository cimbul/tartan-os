#!/bin/bash

set -e

this_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

mkdir -p "$this_dir/aml"

for asl_file in "$this_dir"/asl/*.asl; do
    aml_file="$this_dir/aml/${asl_file#$this_dir/asl/}"

    # Options:
    #   -oa  Disable optimizations to make sure we can test syntax constructions that the
    #        compiler might otherwise fold/eliminate
    #   -ot  Disable typechecking, because we're interested in syntax, not runtime errors
    #   -vw  Disable "result not used" error for expressions in statement position
    iasl -oa -ot -vw 6114 -p "$aml_file" "$asl_file"
done
