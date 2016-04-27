#!/bin/bash

#For change or replace annoying strings ---> sed -i 's/foo/bar/g' files <---


join_all() {
    local file=$1
    shift

    awk '{print $1, $2}' "$file" | {
        if (($# > 0)); then
            join2 - <(join_all "$@") $(($# + 1))
        else
            cat
        fi
    }
}

join2() {
    local file1=$1
    local file2=$2
    local count=$3

    local fields=$(eval echo 2.{2..$count})
    join -a1 -a2 -e 'NA' -o "0 1.2 $fields" "$file1" "$file2"
}

join_all "$@"
