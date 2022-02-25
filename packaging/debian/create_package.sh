#!/bin/sh -e

# only execute anything if either
# - running under orb with package = jackline
# - not running under opam at all
if [ "$ORB_BUILDING_PACKAGE" != "jackline" -a "$OPAM_PACKAGE_NAME" != "" ]; then
    exit 0;
fi

basedir=$(realpath "$(dirname "$0")"/../..)
bdir=$basedir/_build/install/default/bin
tmpd=$basedir/_build/stage
rootdir=$tmpd/rootdir
bindir=$rootdir/usr/bin
debiandir=$rootdir/DEBIAN

trap 'rm -rf $tmpd' 0 INT EXIT

mkdir -p "$bindir" "$debiandir"

# stage app binaries
install $bdir/jackline $bindir/jackline

# install debian metadata
install -m 0644 $basedir/packaging/debian/control $debiandir/control
install -m 0644 $basedir/packaging/debian/changelog $debiandir/changelog
install -m 0644 $basedir/packaging/debian/copyright $debiandir/copyright

ARCH=$(dpkg-architecture -q DEB_TARGET_ARCH)
sed -i -e "s/^Architecture:.*/Architecture: ${ARCH}/" $debiandir/control

dpkg-deb --build $rootdir $basedir/jackline.deb
echo 'bin: [ "jackline.deb" ]' > $basedir/jackline.install
echo 'doc: [ "README.md" ]' >> $basedir/jackline.install
