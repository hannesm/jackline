
.PHONY: debian clean

build: src/* cli/* bin/*
	ocaml pkg/pkg.ml build

clean:
	ocaml pkg/pkg.ml clean
	rm -fr debian/.debhelper/ .debian/jackline/ .debian/jackline.substvars

debian-signed:
	# to use this target, you must have 'git-buildpackage' installed
	gbp dch --id-length=8 -R --commit --commit-msg="debian: Update changelog for jackline_%(version)s.deb release"
	gbp buildpackage --git-ignore-branch --git-no-create-orig \
          --git-builder="debuild --rootcmd=env --preserve-envvar=PATH -b"

debian-test:
	# to use this target, you must have 'git-buildpackage' installed
	gbp buildpackage --git-ignore-branch --git-no-create-orig \
          --git-no-sign-tags \
          --git-builder="debuild --rootcmd=env --preserve-envvar=PATH -b -us -uc"
