
.PHONY: debian clean

build: src/* cli/* bin/*
	cd ../ && ocaml pkg/pkg.ml build

clean:
	cd ../ && ocaml pkg/pkg.ml clean
	rm -fr debian/.debhelper/ .debian/jackline/ .debian/jackline.substvars

debian-signed:
	# to use this target, you must have 'git-buildpackage' installed
	cd ../ && gbp dch --id-length=8 -R --commit --commit-msg="debian: Update changelog for jackline_%(version)s.deb release"
	gbp buildpackage --git-ignore-branch --git-no-create-orig \
          --git-builder="debuild --preserve-envvar=PATH -b"

debian-test:
	# to use this target, you must have 'git-buildpackage' installed
	cd ../ && gbp buildpackage --git-ignore-branch --git-no-create-orig \
          --git-no-sign-tags \
          --git-builder="debuild --preserve-envvar=PATH -b -us -uc"
