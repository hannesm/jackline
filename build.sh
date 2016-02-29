#!/bin/sh
# This script is only used for developement. It is removed by the
# distribution process.

set -e

OCAMLBUILD=${OCAMLBUILD:="ocamlbuild -tag debug -classic-display \
                          -use-ocamlfind" }

action ()
{
    case $1 in
        default)
            GIT="$(git log --abbrev-commit --oneline -1 | tr -d '/' \
                 | sed -e 's:\\:\\\\\\\\:g' -e 's#"#\\\\"#g' )"
            DIRTY=$(git status -bs --porcelain | wc -l)
            if [ $DIRTY -eq 1 ]; then
                cat src/utils.ml | sed -e "s/%%VERSION%%/$GIT/g" > src/utils.ml.tmp
            else
                cat src/utils.ml | sed -e "s/%%VERSION%%/$GIT (dirty)/g" > src/utils.ml.tmp
            fi
            mv src/utils.ml.tmp src/utils.ml ;
            action lib ;
            git checkout src/utils.ml ;;
        lib) $OCAMLBUILD src/xmpp_client.cmx cli/xmpp_client_cli.cmx bin/jackline.native ;;
        clean)   $OCAMLBUILD -clean ;;
        *)       $OCAMLBUILD $* ;;
    esac
}

if [ $# -eq 0 ];
then action default ;
else action $*; fi
