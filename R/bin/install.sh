#!/bin/sh

exit_error() {
    echo "Error:$*" >&2
    exit 1
}

check_setup() {
    if [ ! -d "$base" ] ; then
        exit_error "Cannot find base directory"
    elif [ ! -d "$packages" ] ; then
        exit_error "Cannot locate package directory"
    fi
    for p in "$packages"/* ; do
        if [ ! -d "$p" ] ; then
            exit_error "Not a single package found for installation"
        fi
        break
    done
    "${rexec}" --slave -e "1" >/dev/null || exit_error "Cannot run a simple R program"
}

install_package() {
    true
}

rexec="R"
launch=`dirname "$0"`
base="$launch/.."
packages="$base/packages"
libpath=""

while getopts ":R:l:" opt ; do
    case "$opt" in
        R) rexec="$OPTARG" ;;
        l) libpath="$OPTARG" ;; 
        '?') exit_error "Invalid option:-$OPTARG" ;;
        :) exit_error "Option -$OPTARG requires argument" ;;
    esac
done
check_setup

if [ -n "$libpath" ] ; then
    "$rexec" CMD INSTALL -l "$libpath" "$packages"/*
else
    "$rexec" CMD INSTALL "$packages"/*
fi
if [ $? -ne 0 ] ; then
    exit_error "Error installing packages."
fi

echo "DONE" >&2
