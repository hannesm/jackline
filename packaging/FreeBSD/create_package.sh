#!/bin/sh -e

# only execute anything if either
# - running under orb with package = jackline
# - not running under opam at all
if [ "$ORB_BUILDING_PACKAGE" != "jackline" -a "$OPAM_PACKAGE_NAME" != "" ]; then
    exit 0;
fi

basedir=$(realpath "$(dirname "$0")"/../..)
pdir=$basedir/packaging/FreeBSD
bdir=$basedir/_build/install/default/bin
tmpd=$basedir/_build/stage
manifest=$tmpd/+MANIFEST
rootdir=$tmpd/rootdir
bindir=$rootdir/usr/local/bin

trap 'rm -rf $tmpd' 0 INT EXIT

mkdir -p "$bindir"

# stage app binaries
install -U $bdir/jackline $bindir/jackline

# create +MANIFEST
flatsize=$(find "$rootdir" -type f -exec stat -f %z {} + |
               awk 'BEGIN {s=0} {s+=$1} END {print s}')

sed -e "s:%%FLATSIZE%%:${flatsize}:" "$pdir/MANIFEST" > "$manifest"

{
    printf '\nfiles {\n'
    find "$rootdir" -type f -exec sha256 -r {} + | sort |
        awk '{print "    " $2 ": \"" $1 "\"," }'
    find "$rootdir" -type l | sort |
        awk "{print \"    \"\$1 \": -,\"}"
    printf '}\n'
} | sed -e "s:${rootdir}::" >> "$manifest"

export SOURCE_DATE_EPOCH=$(git log -1 --pretty=format:%ct)
pkg create -r "$rootdir" -M "$manifest" -o $basedir/
mv $basedir/jackline-*.pkg $basedir/jackline.pkg
echo 'bin: [ "jackline.pkg" ]' > $basedir/jackline.install
echo 'doc: [ "README.md" ]' >> $basedir/jackline.install
