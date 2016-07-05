#!/bin/sh
# This script is only used for developement. It is removed by the
# distribution process.

set -e

OCAMLBUILD=${OCAMLBUILD:="ocamlbuild -tag debug -classic-display \
                          -use-ocamlfind" }
OCAMLDOCFLAGS=${OCAMLDOCFLAGS:="-docflags -colorize-code,-charset,utf-8"}
BUILDDIR=${BUILDDIR:="_build"}

# hack to avoid depending on result package
RESULT="src/core.ml src/core.mli"
V=$(ocamlc -version)
MAJOR=$(echo $V | cut -d '.' -f 1)
MINOR=$(echo $V | cut -d '.' -f 2)
if [ $MAJOR -ge 4 -a $MINOR -ge 3 ] ; then
    for x in $RESULT ; do
        cat $x | grep -v "type ('a, 'b) result" > $x.tmp
        mv $x.tmp $x
    done
fi

action ()
{
    case $1 in
        default) action bin ; action test ;;
        bin) action lib ; $OCAMLBUILD app/conex.native ;;
        lib) $OCAMLBUILD conex.cmx conex.cmxa ;;
        test) action lib ; $OCAMLBUILD test/tests.native ;;
        doc) shift
             $OCAMLBUILD -no-links $OCAMLDOCFLAGS doc/api.docdir/index.html
             cp doc/style.css $BUILDDIR/$DOCDIRFILE/style.css ;;
        clean) $OCAMLBUILD -clean ; rm -rf _tests ; rm -f keys.native ; rm -f conex.native ;;
        *) $OCAMLBUILD $* ;;
    esac
}

if [ $# -eq 0 ];
then action default ;
else action $*; fi

# hack to avoid depending on result package
if [ $MAJOR -ge 4 -a $MINOR -ge 3 ] ; then
    for x in $RESULT ; do
        echo "type ('a, 'b) result = Ok of 'a | Error of 'b" > $x.tmp
        cat $x >> $x.tmp
        mv $x.tmp $x
    done
fi
