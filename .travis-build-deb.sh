#!/bin/sh
# This script builds Debian source and binary packages on Travis.
set -xe
# Build the source package in ../.
debian/rules x-source
# Extract the generated package version number from debian/changelog as we
# need it to locate the built source and binary packages.
VERSION="$(dpkg-parsechangelog --show-field Version)"
mkdir tmp
cd tmp
# Extract the source package in tmp/.
dpkg-source -x "../../jackline_${VERSION}.dsc"
cd "jackline-${VERSION}"
# Install its build dependencies (deduced from debian/control).
sudo mk-build-deps --install --tool='apt-get --no-install-recommends --yes' debian/control
# Now go on to build the binary .deb package.
debuild -us -uc
[ -f "../jackline_${VERSION}_amd64.deb" ]
